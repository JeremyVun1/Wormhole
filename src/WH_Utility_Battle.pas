unit WH_Utility_Battle;

///////////
//public
///////////
interface

uses SwinGame, sgTypes, WH_Types_Battle, WH_Types_Shared, WH_Settings, WH_Utility_Shared, Typinfo, sgBackEndTypes;

//translates a grid value in terms of line length into an x,y point
//BoxGridToPoint(Point2D): Point2D
function BoxGridToPoint(Point: Point2D): Point2D;

//removes the specified entry index from the array
//Debris
procedure RemoveIndex(var DebrisList: DebrisListarray; Index: Integer); overload;
//Inventory
procedure RemoveIndex(var Inventory: InventoryArray; Index: Integer); overload;
//NumberPopups
procedure RemoveIndex(var NumberPopups: NumberPopupArray; Index: Integer); overload;
//Loot
procedure RemoveIndex(var LootList: LootDataArray; Index: Integer); overload;
//Ammo
procedure RemoveIndex(var Ammoarray: AmmoListArray; Index: Integer); overload;
//Particles
procedure RemoveIndex(var Particlearray: ParticleDataArray; Index: Integer);
//Ships
procedure RemoveIndex(var ShipArray: ShipDataArray; Index: Integer);

//returns the difficulty modifier of the passed in difficulty type
//GetDifficultyModifier(DifficultyType): Single;
function GetDifficultyModifier(const Difficulty: DifficultyType): Single;

//returns a random vector within the passed in maximums
//GetRandomVector(Integer, Integer): Vector
function GetRandomVector(xMax, yMax: Integer): Vector;

//Returns a damage modifier based on the state of the damage powerup flag
//GetDamageBuffModifier(Boolean): Single
function GetDamageBuffModifier(Switch: Boolean): Single;

//Returns a speed modifier depending on the state of the speed powerup flag
//GetSpeedBuffMod(Boolean): Single
function GetSpeedBuffMod(Switch: Boolean): Single;

//Returns a random point2d within the play area around the origin (-PLAY_WIDTH/2 to +PLAY_WIDTH/2)
//Option to offset by a percentage (0.5 offsets by 50%);
//GetRandomPointWithin(single, single, single): Point2D;
function GetRandomPointWithin(w, h: Single; OffsetOpt: Single = 0): Point2D;

//Returns base damage for the passed in ammotype
//GetToolDamage(AmmoType): Single
function GetToolDamage(AmmoKind: AmmoType): Single;

//Returns the base reload time for the pased in ammotype
//GetToolReloadTime(ToolData): Single
function GetToolReloadTime(AmmoKind: AmmoType): Single;

//Returns true if the timer has passed the threshhold value or if the timer has not yet been started
//TimerAllowsAction(Timer, Single): Boolean
function TimerAllowsAction(const Clock: Timer; Threshold: Single): Boolean;

//Returns whether Invincible buff is active
//IsInvincible(BuffData): Boolean
function IsInvincible(const Powerup: BuffData): Boolean;

//returns whether damage buff is active
//IsDamageBuff(BuffData): Boolean
function IsDamageBuff(const Powerup: BuffData): Boolean;

//returns whether speed buff is active
//IsSpeedBuff(BuffData): Boolean
function IsSpeedBuff(const Powerup: BuffData): Boolean;

//Returns a rotated vector by the specified angle in radians
//RotateVector(Vector, Single): Vector
function RotateVector(Vec: Vector; Theta: Single): Vector;

//multiplies the passed in vector by a number
//MultiplyVector(Vector, Single): Vector
function MultiplyVector(Vec: Vector; Num: Single): Vector;

//Rotates the point by angle in radians around the passed in anchor of rotation
//RotatePoint(Point, Single, Single, Single)
procedure RotatePoint(var  Point: Point2D; Theta, AnchorX, AnchorY: Single);

//Rotates the passed in shape by theta(radians) around the passed in rotation anchor point
//RotateShape(LinesArray, Single, Single, Single)
procedure RotateShape(var Shape: LinesArray; const Theta: Double; AnchorX: Single = 0; AnchorY: Single = 0);

///Moves a Point by the passed in vector
//MovePoint(Point2D, Vector)
procedure MovePoint(var Point: Point2D; const Vector: Vector);

//Moves a Line Segment by the passed in vector
//MoveLineSegment(LineSegment, Vector)
procedure MoveLineSegment(var Line: LineSegment; const Vector: Vector);

//Move an array of line segments by the passed in vector
//MoveShape(LinesArray, Vector)
procedure MoveShape(var Shape: LinesArray; const Vector: Vector);

//teleports a shape to the passed in x, y coordinate
//TeleportShape(LinesArray, Single, Single, Point2D)
procedure TeleportShape(var Shape: LinesArray; const x, y: Single; const Pos: Point2D);

//Moves an array of Points by the passed in vector
//TeleportPointArray(Point2DArray, Single, Signle, Point2D)
procedure TeleportPointArray(var PointArray: Point2DArray; const x, y: Single; const Pos: Point2D);

//teleports ship to the specified x,y coord
//TeleportShip(ShipData, Single, Single)
procedure TeleportShip(var Ship: ShipData; const x, y: Single);

//Applies spin decay to a turn rate by Count times
//DecayRotation(Double, Integer)
procedure DecayRotation(var TurnRate: Double; Count: Integer = 1);

//Applies speed decay to a vector by Count times
//DecayVelocity(Vector, Integer)
procedure DecayVelocity(var Vel: Vector; Count: Integer = 1);

//returns whether there is a collision between LinesArrays
//CollisionBetweenShapes(LinesArray, LinesArray): Boolean
function CollisionBetweenShapes(const Shape1: LinesArray; const Shape2: LinesArray): Boolean;

//Returns whether the player has space in their inventory
//InvHasSpace(InventoryArray): Boolean
function InvHasSpace(const Inventory: InventoryArray): Boolean;

//Returns the index of an empty inventory slot
//GetEmptyInvIndex(InventoryArray): Integer
function GetEmptyInvIndex(const Inventory: InventoryArray): Integer;

//Returns whether an Item can be activated by the player or not
//CanActivateItem(ItemData, BuffData): Boolean
function CanActivateItem(const Item: ItemData; const Powerup: BuffData): Boolean;

//Returns the bitmap of the specified item type
//GetItemBmp(ItemType): Bitmap
function GetItemBmp(const ItemKind: ItemType): Bitmap;

//Returns the max health of the passed in shipkind
//GetShipMaxHealth(ShipType): Integer
function GetShipMaxHealth(const ShipKind: ShipType): Integer;

//Returns a random starting position that isn't in or near the wormhole
//GetStartingPosition(): Point2D
function GetStartingPosition(): Point2D;

//Setup Emitter Pointers to parent ship
procedure SetupEmitterPointers(var Emitters: EmitterDataArray; const NumOfAnchors: Integer; var AnchorPoints: Point2DArray; var Heading: Vector);

//Setup Tool Pointers to parent ship
procedure SetupToolPointers(var Tools: ToolDataArray; const NumOfAnchors: Integer; var AnchorPoints: Point2DArray; var Heading: Vector);

//Activates the named emitter/s
//ActivateEmittersNamed(array of emitter, String)
procedure ActivateEmittersNamed(var Emitter: array of EmitterData; EmitterName: String);

//creates a number popup record for drawing to screen
//SpawnNumberPopup(Single, PopupType, Single, Single, NumberPopupPtr);
procedure SpawnNumberPopup(const Num: Single; const PopupKind: NumberPopupType; const Pos: Point2D; const NumberPopups: NumberPopupPtr);

//returns the type modifier of the passed in ship type
//GetTypeModifier(ShipType): Single
function GetTypeModifier(const ShipKind: ShipType): Single;

//Turns the timer package on
//StartTimerPackage(TimerPackage)
procedure StartTimerPackage(var Package: TimerPackage);

//Turns the timer package off
//StopTimerPackage(TimerPackage)
procedure StopTimerPackage(var Package: TimerPackage);

//frees the timers in the timer package
//ReleaseTimerPackage(TimerPackage)
procedure ReleaseTimerPackage(var Package: TimerPackage);

//returns the maximum dps the array of tools can output
//ToolarrayMaxDps(array of ToolData): Single
function PlayerMaxDps(const Tools: array of ToolData): Single;

//Update the position of the ship, its children line segments, and any anchor points based on its Velocity vector
//ApplyVelocity(MovementModel, LinesArray, Boolean)
procedure ApplyVelocity(var Move: MovementModel; var Shape: LinesArray; SpeedBuff: Boolean = False);

//Clamps Velocity to it's Max Velocity in any direction
//ClampVelocity(Ship, Boolean)
procedure ClampVelocity(var Move: MovementModel; SpeedBuff: Boolean = False);

//apply friction to the ship if no player input detected.
//ApplyFriction(MovementModel)
procedure ApplyFriction(var Move: MovementModel);

//makes sure ship pointers are pointing at the correct location
//AssertShipPointers(ShipDataArray, Integer)
procedure AssertShipPointers(var Ship: ShipData);

//Iterate backwards through the array to remove dead and expired entities
//Inventory Items
procedure RemoveDead(var Inventory: InventoryArray);
//Number popups
procedure RemoveDead(var NumberPopups: NumberPopupArray);
//Loot
procedure RemoveDead(var LootList: LootDataArray); Overload;
//Ammo
procedure RemoveDead(var SpawnedAmmo: AmmoListArray); Overload;
//Particles
procedure RemoveDead(var Particlearray: ParticleDataArray); Overload;
//Debris
procedure RemoveDead(var DebrisList: DebrisListarray); Overload;
//Ships
procedure RemoveDead(var ShipArray: ShipDataArray); Overload;

//returns whether player has made a command input
function StrafeLeftInput(): Boolean;
function StrafeRightInput(): Boolean;
function AccelerateForwardInput(): Boolean;
function AccelerateBackwardInput(): Boolean;
function RotateRightInput(): Boolean;
function RotateLeftInput(): Boolean;
function FireBallisticInput(): Boolean;
function ActivatePowerupInput(): Boolean;

//Returns the tier color from the passed in NPCTier Index
//GetNPCTierColor(Integer): LongWord
function GetNPCTierColor(Index: Integer): LongWord;

//returns an empty inventory record
//GetEmptyInventory(): InventoryArray
function GetEmptyInventory(): InventoryArray;

///////////
//private
///////////
implementation

function BoxGridToPoint(Point: Point2D): Point2D;
var
	centering: Single;
begin
	centering := LINE_LENGTH/2;
	Point.x *= LINE_LENGTH;
	Point.y *= LINE_LENGTH;
	Point.x -= centering;
	Point.y -= centering;

	Result := Point;
end;

procedure RemoveIndex(var DebrisList: DebrisListarray; Index: Integer); overload;
var
	i: Integer;
begin
	if (Index < Low(DebrisList)) then Exit
	else
	begin
		for i:=Index to (High(DebrisList)-1) do
		begin
			DebrisList[i] := DebrisList[i+1];
		end;
		SetLength(DebrisList, Length(DebrisList)-1);
	end;
end;

procedure RemoveIndex(var Inventory: InventoryArray; Index: Integer); overload;
var
	i: Integer;
begin
	if (Index < Low(Inventory)) then Exit
	else
	begin
		for i:=Index to (High(Inventory)-1) do
		begin
			Inventory[i] := Inventory[i+1];
		end;
		Inventory[High(Inventory)].Drained := True;
	end;
end;

procedure RemoveIndex(var NumberPopups: NumberPopupArray; Index: Integer); overload;
var
	i: Integer;
begin
	if (Index < Low(NumberPopups)) then Exit
	else
	begin
		for i:=Index to (High(NumberPopups)-1) do
		begin
			NumberPopups[i] := NumberPopups[i+1];
		end;
		SetLength(NumberPopups, (Length(NumberPopups)-1));
	end;
end;

procedure RemoveIndex(var LootList: LootDataArray; Index: Integer); overload;
var
	i: Integer;
begin
	if (Index < Low(LootList)) then Exit
	else
	begin
		for i:=Index to (High(LootList)-1) do
		begin
			LootList[i] := LootList[i+1];
		end;
		SetLength(LootList, (Length(LootList)-1));
	end;
end;

procedure RemoveIndex(var Ammoarray: AmmoListArray; Index: Integer); overload;
var
	i: Integer;
begin
	if (Index < Low(Ammoarray)) then Exit
	else
	begin
		for i:=Index to (High(Ammoarray)-1) do
		begin
			Ammoarray[i] := Ammoarray[i+1];
		end;
		SetLength(Ammoarray, (Length(Ammoarray)-1));
	end;
end;

procedure RemoveIndex(var Particlearray: ParticleDataArray; Index: Integer);
var
	i: Integer;
begin
	if (Index < Low(Particlearray)) then Exit
	else
	begin
		for i:=Index to (High(Particlearray)-1) do
		begin
			Particlearray[i] := Particlearray[i+1];
		end;
		SetLength(Particlearray, Length(Particlearray)-1);
	end;
end;

procedure RemoveIndex(var ShipArray: ShipDataArray; Index: Integer);
var
	i: Integer;
begin
	if (Index < Low(ShipArray)) then Exit
	else
	begin
		for i:=Index to (High(ShipArray)-1) do
		begin
			ShipArray[i] := ShipArray[i+1];
		end;
		SetLength(ShipArray, Length(ShipArray)-1);
	end;
end;

function GetDifficultyModifier(const Difficulty: DifficultyType): Single;
begin
	Result := 1;
	case Difficulty of
		Novice: Result := NOVICE_MODIFIER;
		Intermediate: Result := INTERMEDIATE_MODIFIER;
		Expert: Result := EXPERT_MODIFIER;
		else WriteLn('GetDifficultyModifier() - invalid Difficulty Kind');
	end;
end;

function GetRandomVector(xMax, yMax: Integer): Vector;
begin
	Result := VectorTo((Random(200*xMax)-100*xMax)/100, (Random(200*yMax)-100*yMax)/100);
end;

function GetDamageBuffModifier(Switch: Boolean): Single;
begin
	Result := 1;
	if (Switch) then Result := BUFF_DAMAGE_MODIFIER;
end;

function GetSpeedBuffMod(Switch: Boolean): Single;
begin
	Result := 1;
	if (Switch) then Result := BUFF_SPEED_MODIFIER;
end;

function GetRandomPointWithin(w, h: Single; OffsetOpt: Single = 0): Point2D;
begin
	Result.x := Random(Round(w)) - w/2 + (OffsetOpt * w);
	Result.y := Random(Round(h)) - h/2 + (OffsetOpt * h);
end;

function GetToolDamage(AmmoKind: AmmoType): Single;
begin
	Result := 1;
	case AmmoKind of
		Ballistic: Result := BALLISTIC_DAMAGE;
		//Missile: Result := MISSILE_DAMAGE;
	end;
end;

function GetToolReloadTime(AmmoKind: AmmoType): Single;
begin
	Result := 1;
	case AmmoKind of
		Ballistic: Result := 1/BALLISTIC_FIRE_RATE;
		//Missile: Result := 1/MISSILE_FIRE_RATE;
	end;
end;

function TimerAllowsAction(const Clock: Timer; Threshold: Single): Boolean;
begin
	if (GetTimerSeconds(Clock) > Threshold) or (TimerTicks(Clock) = 0) then
	begin
		result := true;
	end
	else Result := false;
end;

function IsInvincible(const Powerup: BuffData): Boolean;
begin
	Result := Powerup.Invincible.Switch;
end;

function IsDamageBuff(const Powerup: BuffData): Boolean;
begin
	Result := Powerup.Damage.Switch;
end;

function IsSpeedBuff(const Powerup: BuffData): Boolean;
begin
	Result := Powerup.Speed.Switch;
end;

function RotateVector(Vec: Vector; Theta: Single): Vector;
begin
	Result.x := Vec.x*Cos(Theta) - Vec.y*Sin(Theta);
	Result.y := Vec.y*Cos(Theta) + Vec.x*Sin(Theta);
end;

function MultiplyVector(Vec: Vector; Num: Single): Vector;
begin
	Vec.x *= Num;
	Vec.y *= Num;
	Result := Vec;
end;

procedure RotatePoint(var  Point: Point2D; Theta, AnchorX, AnchorY: Single);
var
	x1, y1: Single;
begin
	//rotate by formula
	//x'=x*Cos(t) - y*Sin(t)
	//y'=y*Cos(t) + x*Sin(t)
	x1 := Point.x - AnchorX;
	y1 := Point.y - AnchorY;
	Point.x := x1*Cos(Theta) - y1*Sin(Theta) + AnchorX;
	Point.y := y1*Cos(Theta) + x1*Sin(Theta) + AnchorY;
end;

procedure RotateShape(var Shape: LinesArray; const Theta: Double; AnchorX: Single = 0; AnchorY: Single = 0);
var
	i : Integer;
begin
	for i:=0 to High(Shape) do
	begin
		RotatePoint(Shape[i].StartPoint, theta, AnchorX, AnchorY);
		RotatePoint(Shape[i].EndPoint, theta, AnchorX, AnchorY);
	end;
end;

procedure MovePoint(var Point: Point2D; const Vector: Vector);
begin
	Point.x += Vector.x;
	Point.y += Vector.y;
end;

procedure MoveLineSegment(var Line: LineSegment; const Vector: Vector);
begin
	MovePoint(Line.StartPoint, Vector);
	MovePoint(Line.EndPoint, Vector);
end;

procedure MoveShape(var Shape: LinesArray; const Vector: Vector);
var
	i: Integer;
begin
	for i:=0 to High(Shape) do
	begin
		MoveLineSegment(Shape[i], Vector);
	end;
end;

procedure TeleportShape(var Shape: LinesArray; const x, y: Single; const Pos: Point2D);
var
	i: Integer;
	MoveBy: Vector;
begin
	MoveBy.x := x - Pos.x;
	MoveBy.y := y - Pos.y;

	for i:=0 to High(Shape) do
	begin
		MoveLineSegment(Shape[i], MoveBy);
	end;
end;

procedure TeleportPointArray(var PointArray: Point2DArray; const x, y: Single; const Pos: Point2D);
var
	i: Integer;
	MoveBy: Vector;
begin
	MoveBy.x := x - Pos.x;
	MoveBy.y := y - Pos.y;

	for i:=0 to High(PointArray) do
	begin
		MovePoint(PointArray[i], MoveBy);
	end;
end;

procedure TeleportShip(var Ship: ShipData; const x, y: Single);
begin
	TeleportShape(Ship.Shape, x, y, Ship.Move.Pos);
	TeleportPointArray(Ship.AnchorPoint, x, y, Ship.Move.Pos);
	Ship.Move.Pos := PointAt(x, y);
end;

procedure DecayRotation(var TurnRate: Double; Count: Integer = 1);
var
	i: Integer;
begin
	for i:=1 to Count do
	begin
		if (TurnRate <> 0) then
		begin
			TurnRate *= SPIN_DECAY;
		end;
	end;
end;

procedure DecayVelocity(var Vel: Vector; Count: Integer = 1);
var
	i: Integer;
begin
	for i:=1 to Count do
	begin
		if (Vel.x <> 0) or (Vel.y <> 0) then
		begin
			Vel.x *= SPEED_DECAY;
			Vel.y *= SPEED_DECAY;			
		end;
	end;
end;

function CollisionBetweenShapes(const Shape1: LinesArray; const Shape2: LinesArray): Boolean;
var
	i, j: Integer;
begin
	Result := False;
	for i:=0 to High(Shape1) do
	begin
		for j:=0 to High(Shape2) do
		begin
			if LineSegmentsIntersect(Shape1[i], Shape2[j]) then
			begin
				Result := True;
				Exit;
			end;
		end;
	end;
end;

function InvHasSpace(const Inventory: InventoryArray): Boolean;
var
	i: Integer;
begin
	Result := False;
	for i:=0 to High(Inventory) do
	begin
		if (Inventory[i].Drained = True) then
		begin
			Result := True;
			Exit;
		end;
	end;
end;

function GetEmptyInvIndex(const Inventory: InventoryArray): Integer;
var
	i: Integer;
begin
	Result := 0;
	for i:=0 to High(Inventory) do
	begin
		if (Inventory[i].Drained = True) then
		begin
			Result := i;
			Exit;
		end;
	end;
	writeLn('GetEmptyInvIndex() - No empty inventory Spot');
end;

function CanActivateItem(const Item: ItemData; const Powerup: BuffData): Boolean;
var
	AlreadyActive: Boolean;
begin
	Result := False;
	AlreadyActive := False;

	//check if a corresponding buff is already active
	case Item.ItemKind of
		Invincible: AlreadyActive := IsInvincible(Powerup);
		Speed: AlreadyActive := IsSpeedBuff(Powerup);
		Damage: AlreadyActive := IsDamageBuff(Powerup);
	end;

	if not (AlreadyActive) and not (Item.Drained) then
	begin
		Result := True;
	end;
end;

function GetItemBmp(const ItemKind: ItemType): Bitmap;
begin
	Result :=  BitmapNamed('DamageItem');
	case ItemKind of
		Invincible: Result := BitmapNamed('InvincibleItem');
		Speed: Result := BitmapNamed('SpeedItem');
		Damage: Result := BitmapNamed('DamageItem');
		else WriteLn('GetItemBmp() - Item has no bmp');
	end;
end;

function GetShipMaxHealth(const ShipKind: ShipType): Integer;
begin
	Result := 0;
	case ShipKind of
		LightShip: Result := LIGHT_HEALTH;
		MediumShip: Result := MEDIUM_HEALTH;
		HeavyShip: Result := HEAVY_HEALTH;
		else WriteLn('GetShipMaxHealth() - Invalid ship type');
	end;
end;

function GetStartingPosition(): Point2D;
var
	Wormhole: Circle;
	StartPos: Point2D;
begin
	//0.5 radius padding (x1.5) between wormhole and player spawn position
	Wormhole := CircleAt(0, 0, WORMHOLE_BASE_RADIUS*1.5);
	repeat
		StartPos := GetRandomPointWithin(PLAY_WIDTH * 0.9, PLAY_HEIGHT * 0.9);
	until not PointInCircle(StartPos, Wormhole);

	Result := StartPos;
end;

procedure SetupEmitterPointers(var Emitters: EmitterDataArray; const NumOfAnchors: Integer; var AnchorPoints: Point2DArray; var Heading: Vector);
var
	i, index: Integer;
begin
	for i:=0 to High(Emitters) do
	begin
		if (Emitters[i].AnchorIndex > NumOfAnchors-1) then
		begin
			WriteLn('SetupToolPointers() Error: Invalid anchor index');
		end
		else
		begin
			index := Emitters[i].AnchorIndex;
			Emitters[i].Anchor := @AnchorPoints[index];
			Emitters[i].Heading := @Heading;
		end;
	end;
end;

procedure SetupToolPointers(var Tools: ToolDataArray; const NumOfAnchors: Integer; var AnchorPoints: Point2DArray; var Heading: Vector);
var
	i, index: Integer;
begin
	for i:=0 to High(Tools) do
	begin
		if (Tools[i].AnchorIndex > NumOfAnchors-1) then
		begin
			WriteLn('SetupToolPointers() Error: Invalid anchor index');
		end
		else
		begin
			index := Tools[i].AnchorIndex;
			Tools[i].Anchor := @AnchorPoints[index];
			Tools[i].Heading := @Heading;
		end;
	end;
end;

procedure ActivateEmittersNamed(var Emitter: array of EmitterData; EmitterName: String);
var
	i: Integer;
begin
	for i:=0 to High(Emitter) do
	begin
		if (Emitter[i].EmitterName = Emittername) or (EmitterName='All') then
		begin
			Emitter[i].CoolDown.Switch := True;
			StartTimer(Emitter[i].CoolDown.Clock);
		end;
	end;
end;

procedure SpawnNumberPopup(const Num: Single; const PopupKind: NumberPopupType; const Pos: Point2D; const NumberPopups: NumberPopupPtr);
var
	Clr: LongWord;
	f: Integer;
begin
	SetLength(NumberPopups^, Length(NumberPopups^)+1);
	f := High(NumberPopups^);
	Clr := ColorWhite; //default Color

	//event specific values
	case PopupKind of
		DamagePopup:
		begin
			Clr := ColorYellow;
			NumberPopups^[f].Move.Vel.y := -NUMBER_POPUP_VELOCITY; //number drifts up if damage popup
			NumberPopups^[f].NumberPopupKind := PopupKind;
			NumberPopups^[f].Move.Pos := PointAt(Pos.x+(Random(14)-7), Pos.y-(Random(14)-7));
		end;
		PointsPopup: 
		begin
			Clr := ColorGreen;
			NumberPopups^[f].Move.Vel.y := -NUMBER_POPUP_VELOCITY/4;
			NumberPopups^[f].NumberPopupKind := PopupKind;
			NumberPopups^[f].Move.Pos := PointAt(Pos.x+LINE_LENGTH, Pos.y+LINE_LENGTH);
		end;
	end;

	//general values
	NumberPopups^[f].IsAlive := True;
	NumberPopups^[f].Number := Round(Num);
	NumberPopups^[f].Font := GetEnumName(TypeInfo(NumberPopupType), Ord(PopupKind));
	NumberPopups^[f].Color := Clr;
	NumberPopups^[f].Move.Heading := VectorTo(0,-1);

	//expiry timer
	NumberPopups^[f].Expiry := CreateTimer();
	StartTimer(NumberPopups^[f].Expiry);
end;

function GetTypeModifier(const ShipKind: ShipType): Single;
begin
	Result := 1;
	case ShipKind of
		LightShip: Result := LIGHT_MODIFIER;
		MediumShip: Result := MEDIUM_MODIFIER;
		HeavyShip: Result := HEAVY_MODIFIER;
		else WriteLn('GetTypemodifier() - Invalid ship type');
	end;
end;

procedure StartTimerPackage(var Package: TimerPackage);
begin
	StartTimer(Package.Clock);
	StartTimer(Package.SubClock);
	Package.Switch := True;
end;

procedure StopTimerPackage(var Package: TimerPackage);
begin
	StopTimer(Package.Clock);
	StopTimer(Package.SubClock);
	Package.Switch := False;
end;

procedure ReleaseTimerPackage(var Package: TimerPackage);
begin
	FreeTimer(Package.Clock);
	FreeTimer(Package.SubClock);
end;

function PlayerMaxDps(const Tools: array of ToolData): Single;
var
	i: Integer;
begin
	Result := 0;
	for i:=0 to High(Tools) do
	begin
		Result += GetToolDamage(Tools[i].AmmoKind) * GetToolReloadTime(Tools[i].AmmoKind);
	end;
end;

procedure ApplyVelocity(var Move: MovementModel; var Shape: LinesArray; SpeedBuff: Boolean = False);
begin
	if (High(Shape)>=0) or (abs(Move.Vel.x) + abs(Move.Vel.y) > 0) then
	begin
		//move ship's central reference pposition and its shape
		MovePoint(Move.Pos, MultiplyVector(Move.Vel, GetSpeedBuffMod(SpeedBuff)));
		MoveShape(Shape, MultiplyVector(Move.Vel, GetSpeedBuffMod(SpeedBuff)));
	end
end;

procedure ClampVelocity(var Move: MovementModel; SpeedBuff: Boolean = False);
var
    x, y, c, yxRatio: Double;
begin
	//Clamp straight x and y Vectors
	if (abs(Move.Vel.x) > (Move.MaxVel * GetSpeedBuffMod(SpeedBuff))) then
	begin
		Move.Vel.x := GetSign(Move.Vel.x) * Move.MaxVel * GetSpeedBuffMod(SpeedBuff);
	end;
	if (abs(Move.Vel.y) > (Move.MaxVel * GetSpeedBuffMod(SpeedBuff))) then
	begin
		Move.Vel.y := GetSign(Move.Vel.y) * Move.MaxVel * GetSpeedBuffMod(SpeedBuff);
	end;

    //Clamp (x+y)diagonal Velocities
    //c^2 = x^2 + y^2
	c := sqrt((Move.Vel.x * Move.Vel.x) + (Move.Vel.y * Move.Vel.y));

	if (c > (Move.MaxVel * GetSpeedBuffMod(SpeedBuff))) and (Move.Vel.x <> 0) then
	begin
		//find x and y when c is MAX_VelOCITY
		yxRatio := abs(Move.Vel.y/Move.Vel.x);
		c := Move.MaxVel * GetSpeedBuffMod(SpeedBuff);
		x := sqrt((c*c)/((yxratio*yxratio)+1));
		y := yxRatio*x;

		//Clamp x and y velocity with correct sign
		Move.Vel.x := GetSign(Move.Vel.x) * x;
		Move.Vel.y := GetSign(Move.Vel.y) * y;
	end;
end;

procedure ApplyFriction(var Move: MovementModel);
begin
	if not KeyDown(WKey) and not KeyDown(SKey) and not KeyDown(QKey) and not KeyDown(EKey) then
	begin
		Move.Vel := MultiplyVector(Move.Vel, BASE_FRICTION);
	end;
end;

procedure RemoveDead(var Inventory: InventoryArray);
var
	i: Integer;
begin
	//work backwards from the top of the array so that you don't miss any entries as you iterate
	for i:=High(Inventory) downto 0 do
	begin
		if (Inventory[i].Drained) then
		begin
			RemoveIndex(Inventory, i);
		end;
	end;
end;

procedure RemoveDead(var NumberPopups: NumberPopupArray);
var
	i: Integer;
begin
	for i:=High(NumberPopups) downto 0 do
	begin
		if not (NumberPopups[i].IsAlive) then
		begin
			Freetimer(NumberPopups[i].Expiry);
			RemoveIndex(NumberPopups, i);
		end;
	end;
end;

procedure RemoveDead(var LootList: LootDataArray); Overload;
var
	i: Integer;
begin
	if (High(LootList) >= 0) then
	begin
		for i:=High(LootList) downto 0 do
		begin
			if (LootList[i].PickedUp) then
			begin
				RemoveIndex(LootList, i);
			end;
		end;
	end;
end;

procedure RemoveDead(var SpawnedAmmo: AmmoListArray); Overload;
var
	i: Integer;
begin
	if (High(SpawnedAmmo) >= 0) then
	begin
		for i:=High(SpawnedAmmo) downto 0 do
		begin
			if not (SpawnedAmmo[i].IsAlive) then
			begin
				FreeTimer(SpawnedAmmo[i].Expiry);
				RemoveIndex(SpawnedAmmo, i);
			end;
		end;
	end;
end;

procedure RemoveDead(var Particlearray: ParticleDataArray); Overload;
var
	i: Integer;
begin
	if (High(Particlearray) >= 0) then
	begin
		for i:=High(Particlearray) downto 0 do
		begin
			if not (Particlearray[i].IsAlive) then
			begin
				FreeTimer(ParticleArray[i].Expiry);
				RemoveIndex(Particlearray, i);
			end;
		end;
	end;
end;

procedure RemoveDead(var DebrisList: DebrisListarray); Overload;
var
	i: Integer;
begin
	if (High(DebrisList) >= 0) then
	begin
		for i:=High(DebrisList) downto 0 do
		begin
			if not (DebrisList[i].IsAlive) then
			begin
				RemoveIndex(DebrisList, i);
			end;
		end;
	end;
end;

procedure AssertShipPointers(var Ship: ShipData);
var
	NumOfAnchors: Integer;
begin
	NumOfAnchors := Length(Ship.AnchorPoint);
	SetupToolPointers(Ship.Tool, NumOfAnchors, Ship.AnchorPoint, Ship.Move.Heading);
	SetupEmitterPointers(Ship.Emitter, NumOfAnchors, Ship.AnchorPoint, Ship.Move.Heading);
end;

procedure RemoveDead(var ShipArray: ShipDataArray); Overload;
var
	i, j: Integer;
begin
	for i:=High(ShipArray) downto 0 do
	begin
		if not (ShipArray[i].IsAlive) then
		begin
			RemoveIndex(ShipArray, i);

			//remake pointers because we modified the array
			for j:=0 to High(ShipArray) do
			begin
				AssertShipPointers(ShipArray[j]);
			end;
		end;
	end;
end;

function StrafeLeftInput(): Boolean;
begin
	Result := False;
	If KeyDown(QKey) then
	begin
		Result := True;
	end;
end;

function StrafeRightInput(): Boolean;
begin
	Result := False;
	If KeyDown(EKey) then
	begin
		Result := True;
	end;
end;

function AccelerateForwardInput(): Boolean;
begin
	Result := False;
	If KeyDown(WKey) or KeyDown(UpKey) then
	begin
		Result := True;
	end;
end;

function AccelerateBackwardInput(): Boolean;
begin
	Result := False;
	If KeyDown(SKey) or KeyDown(Downkey) then
	begin
		Result := True;
	end;
end;

function RotateRightInput(): Boolean;
begin
	Result := False;
	If KeyDown(DKey) or KeyDown(RightKey) then
	begin
		Result := True;
	end;
end;

function RotateLeftInput(): Boolean;
begin
	Result := False;
	If KeyDown(AKey) or KeyDown(LeftKey) then
	begin
		Result := True;
	end;
end;

function FireBallisticInput(): Boolean;
begin
	Result := False;
	If KeyDown(SpaceKey) then
	begin
		Result := True;
	end;
end;

function ActivatePowerupInput(): Boolean;
begin
	Result := False;
	If KeyDown(XKey) then 
	begin
		Result := True;
	end;
end;

function GetNPCTierColor(Index: Integer): LongWord;
begin
	case Index of
		0: Result := ColorBurlyWood;
		1: Result := ColorSilver;
		2: Result := ColorGold;
		else Result := ColorBurlyWood;
	end;
end;

function GetEmptyInventory(): InventoryArray;
var
	i: Integer;
begin
	for i:=0 to High(Result) do
	begin
		Result[i].Drained := True;
		Result[i].Activated := False;
	end;
end;

end.