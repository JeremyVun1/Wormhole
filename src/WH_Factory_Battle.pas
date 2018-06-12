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
//CreateBallisticAmmo(): AmmoData
function CreateBallisticAmmo(const Color: LongWord; const Pos: Point2D): AmmoData;

//Ammo Factory interface for creating and returning ammo instances
//CreateAmmo(AmmoType, Vector, Point2D, Single, Single, Owner): AmmoData
function CreateAmmo(const AmmoKind: AmmoType; const Pos: Point2D; const Heading: Vector; const Owner: OwnerType; const Modifier: Single = 1): AmmoData;

//loads the color set for ship collision flashing
//GetShipColors(LongWord): ShipColorArray
function GetShipColors(const BaseColor: LongWord): ShipColorArray;

//Adds a tool to the Ship at the specified anchorpoint with the specified AmmoType
//AddTool(ShipData, Integer, AmmoType)
function AddTool(Tools: ToolDataArray; const AnchorIndex: Integer; const AmmoKind: AmmoType): ToolDataArray;

//Adds an emitter to the Ship at the specified anchorpoint with the specified attributes
//CreateEmitter(ShipData, Integer, String, Single, Single, Single, Single, Double, LongWord, LongWord);
function CreateEmitter(AnchorIndex: Integer; EmitterName: String; Rate, MaxVel, Expiry, Size: Single; TurnRate: Double; Color1, Color2: LongWord): EmitterData;

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

function CreateBallisticAmmo(const Color: LongWord; const Pos: Point2D): AmmoData;
begin
	Result.AmmoKind := Ballistic;
	Result.Move.Accel := 0;
	Result.Move.Vel := VectorTo(BALLISTIC_MAX_VELOCITY, BALLISTIC_MAX_VELOCITY);
	Result.Move.MaxVel := BALLISTIC_MAX_VELOCITY;
	Result.Move.TurnRate := 0;
	Result.Move.Pos := Pos;
	Result.Damage := BALLISTIC_DAMAGE;
	Result.Color := Color;

	//shape
	AddLine(Result.Shape, 0, 0, BALLISTIC_LENGTH, 0, Pos);
end;

function CreateAmmo(const AmmoKind: AmmoType; const Pos: Point2D; const Heading: Vector; const Owner: OwnerType; const Modifier: Single = 1): AmmoData;
var
	theta: Single;
	Color: LongWord;
begin
	Color := ColorYellow; //default

	case AmmoKind of
		Ballistic: Result := CreateBallisticAmmo(Color, Pos);
		//Missile: Ammo := CreateMissileAmmo();
	end;

	//Ammo heading
	//Rotate Ammo to correct heading	
	Result.Move.Heading := Heading;
	theta := VectorAngle(Result.Move.Heading) * (PI/180);
	RotateShape(Result.Shape, theta, Result.Move.Pos.x, Result.Move.Pos.y);

	//general values
	Result.Owner := Owner;
	Result.IsAlive := True;
	Result.Expiry := CreateTimer();
	StartTimer(Result.Expiry);

	//apply modifier
	Result.Move.Vel := MultiplyVector(Heading, Modifier * BALLISTIC_MAX_VELOCITY);
end;

function GetShipColors(const BaseColor: LongWord): ShipColorArray;
begin
	Result[2] := ColorWhite;
	Result[3] := ColorRed;
	Result[1] := BaseColor;
	Result[0] := BaseColor;
end;

function AddTool(Tools: ToolDataArray; const AnchorIndex: Integer; const AmmoKind: AmmoType): ToolDataArray;
var
	f: Integer;
begin
	SetLength(Tools, Length(Tools)+1);
	f := High(Tools);
	
	//setup the tool
	Tools[f].AnchorIndex := AnchorIndex;
	Tools[f].AmmoKind := AmmoKind;
	Tools[f].CoolDown.Clock := CreateTimer();
	Tools[f].Color[0] := ColorWhite;
	Tools[f].Color[1] := ColorRed;

	//tool shape
	AddTriangle(Tools[f].Shape, -LINE_LENGTH/3, LINE_LENGTH/2, LINE_LENGTH/1.5, LINE_LENGTH/2.3, PointAt(0, 0));

	Result := Tools;
end;

function CreateEmitter(AnchorIndex: Integer; EmitterName: String; Rate, MaxVel, Expiry, Size: Single; TurnRate: Double; Color1, Color2: LongWord): EmitterData;
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
	Result.Color[0] := Color1;
	Result.Color[1] := Color2;
end;

function ReadShipEmitters(var ShipFile: TextFile): EmitterDataArray;
var
	i, anchorIndex, count: Integer;
	line, emitterName: String;
	rate, maxVel, expiry, size: Single;
	turnRate: Double;
	color1, color2: LongWord;
begin
	ReadLn(ShipFile, line);
	if (line = 'Emitters') then
	begin
		ReadLn(ShipFile, Count);
		SetLength(Result, Count);
		for i:=0 to (Count-1) do
		begin
			ReadLn(ShipFile, AnchorIndex);
			ReadLn(ShipFile, EmitterName);
			ReadLn(ShipFile, Rate);
			ReadLn(ShipFile, MaxVel);
			ReadLn(ShipFile, Expiry);
			ReadLn(ShipFile, Size);
			ReadLn(ShipFile, TurnRate);
			Color1 := ColorOrange;
			Color2 := ColorCrimson;
			Result[i] := CreateEmitter(anchorIndex, emitterName, rate, maxVel, expiry, size, turnRate, color1, color2);
		end;
	end;
end;

function ReadShipTools(var ShipFile: TextFile): ToolDataArray;
var
	i, anchorIndex, Count: Integer;
	line: String;
	ammoKind: AmmoType;
begin
	ReadLn(ShipFile, line);
	if (line = 'Tools') then
	begin
		ReadLn(ShipFile, Count);
		for i:=0 to (Count-1) do
		begin
			ReadLn(ShipFile, AnchorIndex);
			ReadLn(ShipFile, ammoKind);

			//add tool to the ship
			Result := AddTool(Result, anchorIndex, ammoKind);
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
	TeleportShip(Result, Pos.x, Pos.y);

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