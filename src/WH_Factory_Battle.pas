unit WH_Factory_Battle;

interface

uses SwinGame, sgTypes, SysUtils, sgBackEndTypes, TypInfo, WH_Types_Shared, WH_Types_Battle, WH_Utility_Shared, WH_Utility_Battle, WH_Settings;

//return default, template loot record attributes
//CreateLoot(ItemType): LootData
function CreateLoot(const ItemKind: ItemType): LootData;

//spawns a random loot pickup at the specified coordinates
//SpawnRandomLoot(LootDataArray, LootDataArray, Point2D)
procedure SpawnRandomLoot(var LootList: LootDataArray; const SpawnPoint: Point2D);

//Returns a vector representing the particles velocity
//GetEmittingParticleVector(Single, Vector) Vector
function GetEmittingParticleVector(const MaxVel: Single; const Heading: Vector): Vector;

//Builds and returns a Particle Entity
//CreateParticle(EmitterData): ParticleData
function CreateParticle(const Emitter: EmitterData): ParticleData;

//returns ballistic type ammo
//CreateBallisticAmmo(LongWord, Point2D, Single): AmmoData
function CreateBallisticAmmo(const Color: LongWord; const Pos: Point2D; const Modifier: Single = 1): AmmoData;

//returns a missile type ammo
//CreateMissileAmmo(LongWord, Point2D, Single): AmmoData
function CreateMissileAmmo(const Color: LongWord; const Pos: Point2D; const Modifier: Single = 1): AmmoData;

//Ammo Factory interface for creating and returning ammo instances
//CreateAmmo(AmmoType, Vector, Point2D, Single, Single, Owner): AmmoData
function CreateAmmo(const AmmoKind: AmmoType; const Pos: Point2D; const Heading: Vector; const Owner: OwnerType; const Modifier: Single = 1): AmmoData;

//Returns the appropriate owner color from file
//GetOwnerColor(OwnerColorType);
function GetOwnerColor(const OwnerColorKind: OwnerColorType): LongWord;

//loads the color set for ship collision flashing
//GetShipColors(LongWord): ShipColorArray
function GetShipColors(const BaseColor: LongWord): ShipColorArray;

//Adds a tool to the Ship at the specified anchorpoint with the specified AmmoType
//AddTool(ShipData, Integer, AmmoType)
function AddTool(Tools: ToolDataArray; const AnchorIndex: Integer; const Color: String; const AmmoKind: AmmoType): ToolDataArray;

//Adds an emitter to the Ship at the specified anchorpoint with the specified attributes
//CreateEmitter(ShipData, Integer, String, Single, Single, Single, Single, Double, LongWord, LongWord);
function CreateEmitter(AnchorIndex: Integer; EmitterName: String; Rate, MaxVel, Expiry, Size: Single; TurnRate: Double; Color1, Color2: String): EmitterData;

//creates and returns a timer package (2 timers and a control switch)
//CreateTimerPackage(): TimerPackage;
function CreateTimerPackage(): TimerPackage;

//Apply difficulty modifier to the ships attributes and return a ship entity
//ApplyNPCModifiers(ShipData, DifficultyType): ShipData
function ApplyNPCModifiers(Ship: ShipData; const Difficulty: DifficultyType): ShipData;

//Constructs and returns a ship of the specified kind at the specified Pos x, y coords
//CreateShip(ShipType, LongWWord, OwnerType): ShipData
function CreateShip(const ShipKind: ShipType; const BaseColor: LongWord; const Pos: Point2D; const Owner: OwnerType; const Difficulty: DifficultyType = Novice): ShipData;

//Creates and returns a debris entity from the passed in linesegment
//CreateDebris(LineSegment): Debris
function CreateDebris(const Line: LineSegment; const Color: LongWord): Debris;

//Returns the tier color from the passed in NPCTier Index
//GetNPCTierColor(Integer): LongWord
function GetNPCTierColor(Index: Integer): LongWord;

implementation

//////////////////
// MainGame() Factory functions
//////////////////

function CreateLoot(const ItemKind: ItemType): LootData;
begin
	Result.LootKind := ItemKind;
	Result.Move.Pos := PointAt(0, 0);
	Result.Move.MaxVel := 0;
	Result.Move.TurnRate := LOOT_TURN_RATE * (PI/180);
	Result.Color[0] := ColorGreen;
	Result.Color[1] := ColorBrightGreen;
	Result.PickedUp := False;
	AddRectangle(Result.Shape, -LINE_LENGTH/2, -LINE_LENGTH/2, LINE_LENGTH, LINE_LENGTH, PointAt(0, 0));
end;

procedure SpawnRandomLoot(var LootList: LootDataArray; const SpawnPoint: Point2D);
var
	r, f, numItemTypes: Integer;
	itemKind: ItemType;
begin
	SetLength(LootList, Length(LootList)+1);
	f := High(LootList);

	//create random loot type
	numItemTypes := Ord(High(ItemType)) +1;
	r := Random(numItemTypes);
	itemKind := ItemType(r);
	LootList[f] := CreateLoot(ItemKind);

	//set loot position
	TeleportShape(LootList[f].Shape, SpawnPoint.x, SpawnPoint.y, LootList[f].Move.Pos);
	LootList[f].Move.Pos := SpawnPoint;
end;

function GetEmittingParticleVector(const MaxVel: Single; const Heading: Vector): Vector;
begin
	Result.x := MaxVel * Heading.x * (Random(Round(MaxVel/2)) + (MaxVel/2));
	Result.y := MaxVel * Heading.y * (Random(Round(MaxVel/2)) + (MaxVel/2));
end;

function CreateParticle(const Emitter: EmitterData): ParticleData;
begin
	//initialise particle movement
	Result.Move.Pos := PointAt(Emitter.Anchor^.x + (Random(6)), Emitter.Anchor^.y + (Random(6)));
	Result.Move.Heading := VectorTo(-Emitter.Heading^.x * Random(100)/100, -Emitter.Heading^.y * Random(100)/100);
	Result.Move.Heading := UnitVector(Result.Move.Heading);
	Result.Move.MaxVel := Emitter.MaxVel;
	Result.Move.Vel := GetEmittingParticleVector(Emitter.MaxVel, Result.Move.Heading);
	Result.Move.TurnRate := Random(Round(Emitter.TurnRate*100))/100;
			
	//initialise particle shape and color
	Result.Color := Emitter.Color;
	AddRectangle(Result.Shape, Random(LINE_LENGTH)-LINE_LENGTH, Random(LINE_LENGTH)-LINE_LENGTH, Random(Round(Emitter.Size)), Random(Round(Emitter.Size)), Result.Move.Pos);

	//particle expiry timer
	Result.IsAlive := True;
	Result.Expiry := CreateTimer();
	StartTimer(Result.Expiry);
end;

function MissileAmmoShape(const Pos: Point2D): LinesArray;
begin
	AddRectangle(Result, 0, 0, LINE_LENGTH, LINE_LENGTH/2, Pos);
	AddRectangle(Result, LINE_LENGTH, 0, LINE_LENGTH/3, LINE_LENGTH/2, Pos);
end;

function BallisticAmmoShape(const Pos: Point2D): LinesArray;
begin	
	AddLine(Result, 0, 0, BALLISTIC_LENGTH, 0, Pos);
end;

function CreateMissileAmmo(const Color: LongWord; const Pos: Point2D; const Modifier: Single = 1): AmmoData;
var
	LaunchVel: Single;
begin
	//Movement and Damage
	Result.AmmoKind := Missile;
	Result.Move.Accel := MISSILE_ACCEL * Modifier;
	Result.Move.MaxVel := MISSILE_MAX_VELOCITY * Modifier;
	LaunchVel := Result.Move.MaxVel / 10;
	Result.Move.Vel := VectorTo(LaunchVel, LaunchVel);
	Result.Move.TurnRate := (MISSILE_TURN_RATE/FPS) * (PI/180) * Modifier;
	Result.Move.Pos := Pos;
	Result.Targeting.Seeking := False;
	Result.Targeting.Target := nil;
	Result.Damage := MISSILE_DAMAGE;

	//Shape and color
	Result.Color := Color;	
	Result.Shape := MissileAmmoShape(Pos);

	//AnchorPoint
	SetLength(Result.AnchorPoint, 1);
	Result.AnchorPoint[0] := Pos;
	Result.AnchorPoint[0].x += LINE_LENGTH/4;

	//thrust emitter
	SetLength(Result.Emitter, 1);
	Result.Emitter[0] := CreateEmitter(0, 'Thruster', 45, 1, 0.2, 4, 5, 'Red', 'Yellow');
end;

function CreateBallisticAmmo(const Color: LongWord; const Pos: Point2D; const Modifier: Single = 1): AmmoData;
begin
	//Movement and Damage
	Result.AmmoKind := Ballistic;
	Result.Move.Accel := 0;
	Result.Move.MaxVel := BALLISTIC_MAX_VELOCITY  * Modifier;	
	Result.Move.Vel := VectorTo(Result.Move.MaxVel, Result.Move.MaxVel);
	Result.Move.TurnRate := 0;
	Result.Move.Pos := Pos;
	Result.Targeting.Seeking := False;
	Result.Targeting.Target := nil;
	Result.Damage := BALLISTIC_DAMAGE;

	//Shape and color
	Result.Color := Color;
	Result.Shape := BallisticAmmoShape(Pos);
end;

function CreateAmmo(const AmmoKind: AmmoType; const Pos: Point2D; const Heading: Vector; const Owner: OwnerType; const Modifier: Single = 1): AmmoData;
var
	theta: Single;
	Color: LongWord;
begin
	Color := ColorYellow; //default

	case AmmoKind of
		Ballistic: Result := CreateBallisticAmmo(Color, Pos, Modifier);
		Missile: Result := CreateMissileAmmo(Color, Pos, Modifier);
	end;

	//Ammo heading
	//Rotate Ammo to correct heading	
	Result.Move.Heading := Heading;
	Result.Move.TargetHeading := Heading;
	theta := VectorAngle(Result.Move.Heading) * (PI/180);
	RotateShape(Result.Shape, theta, Result.Move.Pos.x, Result.Move.Pos.y);
	Result.Move.Vel.x *= Heading.x;
	Result.Move.Vel.y *= Heading.y;

	//general values
	Result.Owner := Owner;
	Result.IsAlive := True;
	Result.Expiry := CreateTimer();
	StartTimer(Result.Expiry);
end;

function GetOwnerColor(const OwnerColorKind: OwnerColorType): LongWord;
var
	line, subDir, filename: String;
	ShipColorsFile: TextFile;
begin
	subDir := 'dat\ships\';
	filename := concat(subDir, 'ShipColors.dat');
	Result := ColorWhite;

	If FileExists(PathToResource(filename)) then
	begin
		Assign(ShipColorsFile, PathToResource(filename));
		Reset(ShipColorsFile);
		try
			while not EOF(ShipColorsFile) do
			begin
				ReadLn(ShipColorsFile, line);
				if (line = GetEnumName(TypeInfo(OwnercolorType), ord(OwnerColorKind)))  then
				begin
					ReadLn(ShipColorsFile, line);
					Result := RGBAColorCode(line);
					Close(ShipColorsFile);
					Exit;
				end;
			end;
		except
			WriteLn('GetShipColor() - File Handling Eror');
		end;
		Close(ShipColorsFile);
	end
	else 
	begin
		WriteLn(filename, ' does not exist');
	end;
end;

function GetShipColors(const BaseColor: LongWord): ShipColorArray;
begin
	Result[2] := ColorWhite;
	Result[3] := ColorRed;
	Result[1] := BaseColor;
	Result[0] := BaseColor;
end;

function TurretShape(): LinesArray;
var
	Shape: LinesArray;
begin
	AddTriangle(Shape, -LINE_LENGTH/3, LINE_LENGTH/2, LINE_LENGTH/1.5, LINE_LENGTH/2.3, PointAt(0, 0));
	Result := Shape;
end;

function LauncherShape(): LinesArray;
var
	Shape: LinesArray;
begin
	AddRectangle(Shape, -LINE_LENGTH/4, -LINE_LENGTH/1.5, LINE_LENGTH/2, LINE_LENGTH/1.2, PointAt(0,0));
	Result := Shape;
end;

function AddTool(Tools: ToolDataArray; const AnchorIndex: Integer; const Color: String; const AmmoKind: AmmoType): ToolDataArray;
var
	f: Integer;
begin
	SetLength(Tools, Length(Tools)+1);
	f := High(Tools);
	
	//setup the tool
	Tools[f].AnchorIndex := AnchorIndex;
	Tools[f].AmmoKind := AmmoKind;
	Tools[f].CoolDown.Clock := CreateTimer();
	Tools[f].Color[0] := RGBAColorCode(Color);
	Tools[f].Color[1] := ColorRed;

	//tool shape
	case AmmoKind of 
		Ballistic: Tools[f].Shape := TurretShape();
		Missile: Tools[f].Shape := LauncherShape();
	end;

	Result := Tools;
end;

function CreateEmitter(AnchorIndex: Integer; EmitterName: String; Rate, MaxVel, Expiry, Size: Single; TurnRate: Double; Color1, Color2: String): EmitterData;
begin
	//setup the emitter data
	Result.EmitterName := EmitterName;
	Result.AnchorIndex := AnchorIndex;
	Result.CoolDown.Clock := CreateTimer();
	Result.CoolDown.Switch := False;
	Result.EmissionRate := Rate;
	Result.MaxVel := MaxVel;
	Result.Expiry := Expiry;
	Result.Size := Size;
	Result.TurnRate := TurnRate * (PI/180);
	Result.Color[0] := RGBAColorCode(Color1);
	Result.Color[1] := RGBAColorCode(Color2);
end;

function ReadShipEmitters(var ShipFile: TextFile): EmitterDataArray;
var
	i, anchorIndex, count: Integer;
	line, emitterName, color1Str, color2Str: String;
	rate, maxVel, expiry, size: Single;
	turnRate: Double;
begin
	ReadLn(ShipFile, line);
	if (line = 'Emitters') then
	begin
		ReadLn(ShipFile, Count);
		SetLength(Result, Count);
		for i:=0 to (Count-1) do
		begin
			ReadLn(ShipFile, anchorIndex);
			ReadLn(ShipFile, emitterName);
			ReadLn(ShipFile, rate);
			ReadLn(ShipFile, maxVel);
			ReadLn(ShipFile, expiry);
			ReadLn(ShipFile, size);
			ReadLn(ShipFile, turnRate);
			ReadLn(ShipFile, color1Str);
			ReadLn(ShipFile, color2Str);
			Result[i] := CreateEmitter(anchorIndex, emitterName, rate, maxVel, expiry, size, turnRate, color1Str, color2Str);
		end;
	end;
end;

function ReadShipTools(var ShipFile: TextFile): ToolDataArray;
var
	i, anchorIndex, Count: Integer;
	colorStr, line: String;
	ammoKind: AmmoType;
begin
	ReadLn(ShipFile, line);
	if (line = 'Tools') then
	begin
		ReadLn(ShipFile, Count);
		for i:=0 to (Count-1) do
		begin
			ReadLn(ShipFile, anchorIndex);
			ReadLn(ShipFile, colorStr);
			ReadLn(ShipFile, ammoKind);

			//add tool to the ship
			Result := AddTool(Result, anchorIndex, colorStr, ammoKind);
		end;
	end;
end;

function ReadShipAnchorPoints(var ShipFile: TextFile): Point2DArray;
var
	line: String;
	i, Count: Integer;
	x, y: Single;
begin
	ReadLn(ShipFile, line);
	if (line = 'AnchorPoints') then
	begin
		ReadLn(ShipFile, Count);
		SetLength(Result, Count);
		for i:=0 to (Count-1) do
		begin
			ReadLn(ShipFile, x);
			ReadLn(ShipFile, y);
			Result[i].x := (x * LINE_LENGTH);
			Result[i].y := (y * LINE_LENGTH);
		end;
	end;
end;

function ReadShipExtents(var ShipFile: TextFile): Size;
var
	line: String;
begin
	ReadLn(ShipFile, line);
	if (line = 'Extents') then
	begin
		ReadLn(ShipFile, Result.w);
		ReadLn(ShipFile, Result.h);
		Result.w *= LINE_LENGTH;
		Result.h *= LINE_LENGTH;
	end;
end;

function ReadShipShape(var ShipFile: TextFile): LinesArray;
var
	Count, i: Integer;
	line: String;
	x, y: Single;
	Point: Point2D;
begin
	ReadLn(ShipFile, line);
	if (line = 'Boxes') then
	begin
		ReadLn(ShipFile, Count);
		for i:=0 to (Count-1) do
		begin
			ReadLn(ShipFile, x);
			ReadLn(ShipFile, y);
			Point := BoxGridToPoint(PointAt(x, y));
			AddRectangle(Result, Point.x, Point.y, LINE_LENGTH, LINE_LENGTH, PointAt(0,0));
		end;
	end;

	ReadLn(ShipFile, line);
	if (line = 'Triangles') then
	begin
		ReadLn(ShipFile, Count);
		for i:=0 to (Count-1) do
		begin
			ReadLn(ShipFile, x);
			ReadLn(ShipFile, y);
			Point := BoxGridToPoint(PointAt(x, y));
			AddTriangle(Result, Point.x, Point.y, LINE_LENGTH, LINE_LENGTH, PointAt(0,0));
		end;
	end;
end;

function ReadShipFromFile(const ShipKind: ShipType; var ShipFile: Textfile): ShipData;
var
	FileShipKind: ShipType;
begin
	ReadLn(ShipFile, FileShipKind);

	if (FileShipKind = ShipKind) then
	begin
		//build the ship from file sequentially
		Result.Shipkind := ShipKind;
		Result.Shape := ReadShipShape(ShipFile);
		Result.Extents := ReadShipExtents(ShipFile);
		Result.AnchorPoint := ReadShipAnchorPoints(ShipFile);
		Result.Tool := ReadShipTools(ShipFile);
		Result.Emitter := ReadShipEmitters(ShipFile);
	end
	else WriteLn('ReadLinesFromFile() Wrong Ship Type in File');
end;

function GetShipFromFile(const ShipKind: ShipType): ShipData;
var	
	shipFile: TextFile;
	emptyShip: ShipData;
	subDir, line, fileName: String;
begin
	line := GetEnumName(TypeInfo(ShipType), Ord(ShipKind));
	subDir := 'dat\ships\';
	filename := concat(subDir, line, '.dat');

	If FileExists(PathToResource(filename)) then
	begin
		Assign(shipFile, PathToResource(filename));
		Reset(shipFile);
		try
			Result := ReadShipFromFile(ShipKind, ShipFile);
		except
			WriteLn('File Handling Eror');
		end;
		Close(shipFile);
	end
	else 
	begin
		WriteLn(filename, ' does not exist');
		Result := emptyShip;
	end;
end;

function GetBaseMovementModel(ShipKind: ShipType): MovementModel;
begin
	Result.Heading := VectorTo(0, -1);
	Result.TargetHeading := Result.Heading;
	Result.Vel := VectorTo(0, 0);
	Result.Accel := GetTypeModifier(ShipKind) * BASE_ACCELERATION;
	Result.MaxVel := GetTypeModifier(ShipKind) * BASE_MAX_VELOCITY;
	Result.TurnRate := GetTypeModifier(ShipKind) * ((BASE_TURN_RATE/FPS) * (PI/180)); //convert degrees to radians
	Result.StrafeMod := BASE_STRAFE_MODIFIER * GetTypeModifier(ShipKind);
	Result.ReverseMod := BASE_REVERSE_MODIFIER * GetTypeModifier(ShipKind);
end;

function CreateTimerPackage(): TimerPackage;
begin
	Result.Clock := CreateTimer();
	Result.SubClock := CreateTimer();
	Result.Switch := False;
end;

function GetNPCTierColor(Index: Integer): LongWord;
begin
	case Index of
		0: Result := GetOwnerColor(Tier1Color);
		1: Result := GetOwnerColor(Tier2Color);
		2: Result := GetOwnerColor(Tier3Color);
		else Result := GetOwnerColor(Tier1Color);
	end;
end;

function ApplyNPCModifiers(Ship: ShipData; const Difficulty: DifficultyType): ShipData;
begin
	Ship.Move.TargetHeading := GetRandomVector(1,1);
	Ship.Health *= GetDifficultyModifier(Difficulty);
	Ship.Move.Accel *= GetDifficultyModifier(Difficulty);
	Ship.Move.MaxVel *= GetDifficultyModifier(Difficulty);
	Ship.Move.Vel := GetRandomVector(1,1);

	//movement type
	case random(3) of
		0: Ship.NPCBehaviour := Static;
		1: Ship.NPCBehaviour := Erratic;
		2: Ship.NPCBehaviour := Chase;
	end;

	Result := Ship
end;

function CreateShip(const ShipKind: ShipType; const BaseColor: LongWord; const Pos: Point2D; const Owner: OwnerType; const Difficulty: DifficultyType = Novice): ShipData;
begin
	//Initialise a base ship at the origin
	Result := GetShipFromFile(ShipKind);

	//initialise ship varaibles
	Result.Move := GetBaseMovementModel(ShipKind);
	Result.Health := GetShipMaxHealth(ShipKind);
	Result.Color := GetShipColors(BaseColor);
	Result.Owner := Owner;
	Result.IsAlive := True;

	//Teleport the ship to the passed in Pos x,y
	TeleportEntity(Result.Shape, Result.AnchorPoint, Pos.x, Pos.y, Result.Move.Pos);

	//timers
	Result.Powerup.Invincible := CreateTimerPackage();
	Result.Powerup.Speed := CreateTimerPackage();
	Result.Powerup.Damage := CreateTimerPackage();
	Result.ShipCollide := CreateTimerPackage();
	Result.WormholeCollide := CreateTimerPackage();
	Result.HitByAmmo := CreateTimerPackage();

	if (Owner = Computer) then
	begin
		Result := ApplyNPCModifiers(Result, Difficulty);
	end;
end;

function GetDebrisMovement(const Pos: Point2D): MovementModel;
begin
	Result.Vel.x := (Random(MAX_EXPLOSION_VElOCITY*10)-(MAX_EXPLOSION_VElOCITY*10 div 2))/10;
	Result.Vel.y := (Random(MAX_EXPLOSION_VElOCITY*10)-(MAX_EXPLOSION_VElOCITY*10 div 2))/10;
	Result.MaxVel := MAX_EXPLOSION_VElOCITY div 2;
	Result.TurnRate := ((Random(MAX_EXPLOSION_TURN_RATE)-MAX_EXPLOSION_TURN_RATE) * PI/180);
	Result.Pos := Pos;
	ClampVelocity(Result);
end;

function CreateDebris(const Line: LineSegment; const Color: LongWord): Debris;
begin
	SetLength(Result.Shape, 1);
	Result.Shape[0] := Line;

	//debris specific values
	Result.Move := GetDebrisMovement(LineMidPoint(Result.Shape[0]));
	Result.Color := Color;
	Result.IsAlive := True;
end;

end.