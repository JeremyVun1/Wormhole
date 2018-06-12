program Wormhole;
Uses Math, SwinGame, sgTypes, sgBackEndTypes, GeometryHelper, SysUtils, Typinfo, WH_Menu, WH_Battle, WH_Settings, WH_Utility_Menu, WH_Utility_Battle, WH_Utility_Shared, WH_Types;

type
	OwnerType = (HumanPlayer, Computer);
	ShipType = (LightShip, MediumShip, HeavyShip);
	AmmoType = (Ballistic, Missile);
	ItemType = (Invincible, Speed, Damage);
	NumberPopupType = (DamagePopup, PointsPopup);
	DifficultyType = (Novice, Intermediate, Expert);
	AIType = (Static, Chase, Erratic);
	MenuType = (Root, Select, Options, HighScores, Help, ScoreScreen);
	ButtonAction = (Quit, Save, NavTo, Play, TextInput);
	PayloadType = (MenuKind, Difficulty, ShipClass, Volume, Text, None);


	ShipColorArray = array[0..3] of LongWord;
	Point2DArray = array of Point2D;
	AnchorPtr = ^Point2D;
	AnchorArrayPtr = ^Point2DArray;
	HeadingPtr = ^Vector;
	MenuPtr = ^Menu;

	TimerPackage = record
		Clock: Timer;
		SubClock: Timer;
		Switch: Boolean;
	end;

	Size = record
		w: Single;
		h: Single;
	end;

	MovementModel = record
		Pos: Point2D;
		Heading: Vector;
		TargetHeading: Vector;
		Vel: Vector;
		Accel: Single;
		MaxVel: Single;
		TurnRate: Double;
		StrafeMod: Single;
		ReverseMod: Single;
	end;

	ParticleData = record
		Move: MovementModel;
		IsAlive: Boolean;
		Expiry: Timer;
		Shape: LinesArray;
		Color: array[0..1] of LongWord;
	end;

	ParticleDataArray = array of ParticleData;

	EmitterData = record
		EmitterName: String;
		AnchorIndex: Integer;
		Anchor: AnchorPtr;
		Heading: HeadingPtr;
		MaxVel: Single;
		TurnRate: Double;
		CoolDown: TimerPackage;
		EmissionRate: Single;
		Expiry: Single;
		Color: array[0..1] of LongWord;
		Size: Single;
		Shape: LinesArray;
		Particles: ParticleDataArray
	end;

	EmitterDataArray = array of EmitterData;

	AmmoData = record
		Owner: Ownertype;
		IsAlive: Boolean;
		AmmoKind: AmmoType;
		Move: MovementModel;
		Damage: Single;
		Expiry: Timer;
		Shape: LinesArray;
		Color: LongWord;
	end;

	AmmoListArray = array of AmmoData;

	ToolData = record
		AnchorIndex: Integer;
		Anchor: AnchorPtr;
		Heading: HeadingPtr;
		AmmoKind: AmmoType;
		CoolDown: TimerPackage;
		Color: array[0..1] of LongWord;
		Shape: LinesArray;
		SpawnedAmmo: AmmoListArray;
	end;

	ToolDataArray = array of ToolData;

	BuffData = record
		Invincible: TimerPackage;
		Speed: TimerPackage;
		Damage: TimerPackage;
	end;

	NumberPopupData = record
		NumberPopupKind: NumberPopupType;
		Font: String;
		Number: Integer;
		Color: LongWord;
		Move: MovementModel;
		IsAlive: Boolean;
		Expiry: Timer;
	end;

	NumberPopupArray = array of NumberPopupData;
	NumberPopupPtr = ^NumberPopupArray;

	ShipData = record
		Owner: OwnerType;
		ShipKind : ShipType;
		Extents: Size;
		Move: MovementModel;
		Health: Single;
		IsAlive: Boolean;
		Color: ShipColorArray;
		Shape: LinesArray;
		AnchorPoint: Point2DArray;
		Emitter: EmitterDataArray;
		Tool: ToolDataArray;
		HitByAmmo: TimerPackage;
		ShipCollide: TimerPackage;
		Wormholecollide: TimerPackage;
		NPCBehaviour: AIType;
		PowerUp: BuffData;
	end;

	ShipDataArray = array of ShipData;

	ItemData = record
		ItemKind: ItemType;
		Activated: Boolean;
		Drained: Boolean;
	end;

	InventoryArray = array[0..2] of ItemData;

	UIData = record
		Inventory: array[0..2] of Bitmap;
		SpeedBuffIcon: Bitmap;
		DamageBuffIcon: Bitmap;
		InvincibleBuffIcon: Bitmap;
		HealthContainer: Bitmap;
		WHMeterContainer: Bitmap;
	end;

	Player = record
		Ship: ShipData;
		Inventory: InventoryArray;
		UI: UIData;
		NumberPopups: NumberPopupArray;
	end;

	NPCTier = record
		ShipKind: ShipType;
		Ships: ShipDataArray;
	end;

	NPCTierArray = array of NPCTier;

	Star = record
		Pos: Point2D;
		Size: Single;
	end;

	StarArray = array of Star;

	LootData = record
		LootKind: ItemType;
		Move: MovementModel;
		Shape: LinesArray;
		Color: array[0..1] of LongWord;
		PickedUp: Boolean;
	end;

	LootDataArray = array of lootData;

	WormholeData = record
		Owner: OwnerType;
		IsAlive: Boolean;
		Health: Single;
		Shape: Circle;
		Color: array[0..1] of LongWord;
	end;

	Debris = record
		Move: MovementModel;
		Shape: LinesArray;
		Color: LongWord;
		IsAlive: Boolean;
	end;

	DebrisListArray = array of Debris;

	BackgroundData = record
		Stars: StarArray;
		TwinkleCoolDown: Timer;
	end;

	Level = record
		Background: BackgroundData;
		DebrisList: DebrisListArray;
		LootList: LootDataArray;
		Wormhole: WormholeData;
		Score: Integer;
		SpawnTimer: Timer;
	end;

	Game = record
		LevelData: Level;
		PlayerData: Player;
		NPCData: NPCTierArray;
	end;

	PayloadData = record
		Kind: PayloadType;
		Str: String;
	end;

	ButtonData = record
		ButtonName: String;
		Pos: Point2D;
		Extents: Size;
		Action: ButtonAction;
		Payload: PayloadData;
		Clicked: Boolean;
		Highlighted: Boolean;
		Color: array[0..1] of LongWord;
	end;

	TextBoxData = record
		Text: String;
		Pos: Point2D;
		Color: LongWord;
	end;

	ButtonArray = array of ButtonData;
	TextBoxArray = array of TextBoxData;

	Menu = record
		MenuKind: MenuType;
		MenuName: String;
		Buttons: ButtonArray;
		TextBoxes: TextBoxArray
	end;

	MenuArray = array of Menu;

	MenuData = record
		Menus: MenuArray;
		Background: BackgroundData;
		CurrentMenu: MenuPtr;
	end;

	SendData = record
		Difficulty: DifficultyType;
		ShipClass: ShipType;
	end;

	ReceiveData = record
		Score: Integer;
		Win: Boolean;
		Time: Integer;
	end;

	GameInterfaceData = record
		Send: SendData;
		Receive: ReceiveData;
	end;

const
	//VIEWPORT
	WINDOW_WIDTH = 720;
	WINDOW_HEIGHT = 540;
	PLAY_WIDTH = 1000;
	PLAY_HEIGHT = 1000;
	FPS = 60;

	//DRAWSCALE
	LINE_LENGTH = WINDOW_WIDTH div 65;

	//LEVEL
	STAR_COUNT = PLAY_WIDTH div 5;
	MAX_STAR_SIZE = 5;
	BASE_STAR_SIZE = 1;
	TWINKLE_PORTION = 40;
	TWINKLE_RATE = 6; //per second
	NPC_COUNT = 10;
	NOVICE_MODIFIER = 0.6;
	INTERMEDIATE_MODIFIER = 0.75;
	EXPERT_MODIFIER = 0.9;
	SPIN_DECAY = 0.977;
	SPEED_DECAY = 0.977;
	MAX_EXPLOSION_VELOCITY = 8;
	MAX_EXPLOSION_TURN_RATE = 300; //degrees per second
	LOOT_TURN_RATE = 2; //degrees per second
	PI = 3.1416;
	WORMHOLE_BASE_RADIUS = PLAY_WIDTH/15;
	WORMHOLE_BASE_HEALTH = 60;
	WORMHOLE_CONSTANT_GROWTH = 1; //per second
	WORMHOLE_GROWTH_RATE = 4; //Percent grwoth per second
	NUMBER_POPUP_VELOCITY = 1;
	NUMBER_POPUP_EXPIRY = 4; //in seconds
	AGRO_RANGE = WINDOW_WIDTH/2;
	SPAWN_TIME = 5; // in seconds
	NPC_FIRE_RATE = 0.33; // shots per second

	//SHIP
	LIGHT_MODIFIER = 1.5;
	MEDIUM_MODIFIER = 1.0;
	HEAVY_MODIFIER = 0.6;
	LIGHT_HEALTH = 5;
	MEDIUM_HEALTH = 10;
	HEAVY_HEALTH = 20;
	COLOR_FLASH_RATE = 4; //per second
	BUFF_DURATION = 5; //in seconds
	BUFF_SPEED_MODIFIER = 1.4;
	BUFF_DAMAGE_MODIFIER = 3;

	//MOVEMENT
	BASE_MAX_VELOCITY = WINDOW_WIDTH/270;
	BASE_ACCELERATION = BASE_MAX_VELOCITY/45;
	BASE_TURN_RATE = 160; // degrees per second
	BASE_STRAFE_MODIFIER = 1.2;
	BASE_REVERSE_MODIFIER = 0.8;
	BASE_FRICTION = 0.996;
	PLAYER_SPEED_BUFF = 1.25;
	COLLIDE_TIME_OUT = 2; // seconds
	WORMHOLE_COLLIDE_TIME_OUT = COLLIDE_TIME_OUT/4; // in seconds

	//AMMO
	BALLISTIC_DAMAGE = 1;
	BALLISTIC_FIRE_RATE = 2; //shots per second
	BALLISTIC_MAX_VELOCITY = WINDOW_WIDTH/170;
	BALLISTIC_EXPIRY = 3; //seconds
	BALLISTIC_LENGTH = LINE_LENGTH/4;

//////////////////
// Utility Functions and Procedures
//////////////////

//returns whether the user has clicked the quit button or not
//QuitBtnClicked(MenuType, ButtonArray): Boolean
function QuitBtnClicked(const Buttons: ButtonArray): Boolean;
var
	i: Integer;
begin
	Result := False;
	for i:=0 to High(Buttons) do
	begin
		if (Buttons[i].Action = Quit) and (Buttons[i].Clicked = True) then
		begin
			Result := True;
			Exit;
		end;
	end;
end;

//translates point values from a relative value (percentage of window width and height) to an x,y point
//PercentToPosPoint(Point2D): Point2D
function PercentToPosPoint(Point: Point2D): Point2D;
begin
	Result.x := Point.x * WINDOW_WIDTH;
	Result.y := Point.y * WINDOW_HEIGHT;
end;

//translates size values from relative value (percentage of window width and height) to a w, h size
//PercentToSizeExtent(Size): Size;
function PercentToSizeExtent(Extents: Size): Size;
begin
	Result.w := Extents.w * WINDOW_WIDTH;
	Result.h := Extents.h * WINDOW_HEIGHT;
end;

//returns whether the passed in point is within the specified x, y, width, height box dimensions
//PointWithinBox(Point2D, Point2D, Size): Boolean
function PointWithinBox(Point: Point2D; BoxPos: Point2D; BoxSize: Size): Boolean;
begin
	result := False;
	if (Point.x > BoxPos.x) and (Point.x < (BoxPos.x + BoxSize.w)) and (Point.y > BoxPos.y) and (Point.y < (BoxPos.y + BoxSize.h)) then
	begin
		result := True;
	end;
end;

//returns a pointer to the menu of the specified menukind
//GetMenuOfType(MenuArray, MenuType): MenuPtr
function GetMenuOfType(const Menus: MenuArray; const MenuKind: MenuType): MenuPtr;
var
	i: Integer;
begin
	for i:=0 to High(Menus) do
	begin
		if Menus[i].MenuKind = MenuKind then
		begin
			result := @Menus[i];
			Exit;
		end
	end;

	result := nil;
	WriteLn('GetMenuOfType() - no menu of specified type found, returning empty menu');
end;

//returns whether the score is high enough to be a high score
//IsHighScore(MenuPtr, Integer): Boolean
function IsHighScore(HighScoreMenu: MenuPtr; Score: Integer): Boolean;
var
	i: Integer;
begin
	Result := False;

	for i:=0 to High(HighScoreMenu^.TextBoxes) do
	begin
		if (IntToStr(score) > HighScoreMenu^.TextBoxes[i].Text) then
		begin
			Result := True;
			Exit;
		end;
	end;
end;

//Add a Line segment at the specified coordinates to the LinesArray
//AddLine(LinesArray, Single, Single, Single, Sginle, Point2D)
procedure AddLine(var Shape: LinesArray; x1, y1, x2, y2: Single; const Pos: Point2D);
var
    f: Integer;
begin
	SetLength(Shape, Length(Shape)+1);
	f:=High(Shape);

	Shape[f].StartPoint := PointAt(Pos.x + x1, Pos.y + y1);
	Shape[f].EndPoint := PointAt(Pos.x + x2, Pos.y + y2);
end;

//Add a rectangle shape at the specified coordinates to the LinesArray
//AddRectangle(LinesArray, Single, Single, Single, Single, Point2D)
procedure AddRectangle(var Shape: LinesArray; x, y, w, h: Single; const Pos: Point2D);
begin
	AddLine(Shape, x, y, x+w, y, Pos); //top
	AddLine(Shape, x+w, y, x+w, y+h, Pos); //right
	AddLine(Shape, x+w, y+h, x, y+h, Pos); //bottom
	AddLine(Shape, x, y+h, x, y, Pos); //left
end;

//Add a triangle shape at the specified coordinates to the LinesArray
//AddTriangle(LinesArray, Single, Single, Single, Single, Point2D)
procedure AddTriangle(var Shape: LinesArray; x, y, w, h: Single; const Pos: Point2D);
begin
	AddLine(Shape, x, y, x+w, y, Pos); //base
	AddLine(Shape, x, y, x+(w/2), y-h, Pos); //left diagonal
	AddLine(Shape, x+w, y, x+(w/2), y-h, Pos); //right diagonal
end;

//translates a grid value in terms of line length into an x,y point
//BoxGridToPoint(Point2D): Point2D
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

//removes the specified entry index from the array of Debris
//RemoveIndex(DebrisListarray, Integer)
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

//removes the specified entry index from the array of inventory items
//RemoveIndex(InventoryArray, Integer)
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

//removes the specified entry index from the array of NumberPopups
//RemoveIndex(NumberPopupArray, Integer)
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

//removes the specified entry index from the array of Loot
//RemoveIndex(LootDataArray, Integer)
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

//removes the specified entry index from the array of AmmoData
//RemoveIndex(AmmoListArray, Integer)
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

//removes the specified entry index from the array of particles
//RemoveIndex(ParticleDataArray, Integer)
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

//removes the specified entry index from the array of Ships
//RemoveIndex(ShipDataArray, Integer)
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

//returns the difficulty modifier of the passed in difficulty type
//GetDifficultyModifier(DifficultyType): Single;
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

//Returns integer representation of the sign(+-) of the input parameter
//GetSign(Single): Single
function GetSign(a: Single): Single;
begin
	if (a < 0) then Result := -1
	else if (a > 0) then Result := 1
	else Result := 0;
end;

//clamps x within -MaxNum to +MaxNum
//ClampNumber(Single, Single): Single
function ClampNumber(x, MaxNum: Single): Single;
begin
	Result := x;
	if (x > MaxNum) then
	begin
		Result := MaxNum
	end
	else if (x < -MaxNum) then
	begin
		Result := -MaxNum;
	end;
end;

//returns number of seconds that have elapsed for the passed in timer
//GetTimerSeconds(Timer): Single
function GetTimerSeconds(const Clock: Timer): Single;
begin
	Result := TimerTicks(Clock)/1000;
end;

//returns a random vector within the passed in maximums
//GetRandomVector(Integer, Integer): Vector
function GetRandomVector(xMax, yMax: Integer): Vector;
begin
	Result := VectorTo((Random(200*xMax)-100*xMax)/100, (Random(200*yMax)-100*yMax)/100);
end;

//Returns a damage modifier based on the state of the damage powerup flag
//GetDamageBuffModifier(Boolean): Single
function GetDamageBuffModifier(Switch: Boolean): Single;
begin
	Result := 1;
	if (Switch) then Result := BUFF_DAMAGE_MODIFIER;
end;

//Returns a speed modifier depending on the state of the speed powerup flag
//GetSpeedBuffMod(Boolean): Single
function GetSpeedBuffMod(Switch: Boolean): Single;
begin
	Result := 1;
	if (Switch) then Result := BUFF_SPEED_MODIFIER;
end;

//Returns a random point2d within the play area around the origin (-PLAY_WIDTH/2 to +PLAY_WIDTH/2)
//Option to offset by a percentage (0.5 offsets by 50%);
//GetRandomPointWithin(single, single, single): Point2D;
function GetRandomPointWithin(w, h: Single; OffsetOpt: Single = 0): Point2D;
begin
	Result.x := Random(Round(w)) - w/2 + (OffsetOpt * w);
	Result.y := Random(Round(h)) - h/2 + (OffsetOpt * h);
end;

//Returns base damage for the passed in ammotype
//GetToolDamage(AmmoType): Single
function GetToolDamage(AmmoKind: AmmoType): Single;
begin
	Result := 1;
	case AmmoKind of
		Ballistic: Result := BALLISTIC_DAMAGE;
		//Missile: Result := MISSILE_DAMAGE;
	end;
end;

//Returns the base reload time for the pased in ammotype
//GetToolReloadTime(ToolData): Single
function GetToolReloadTime(AmmoKind: AmmoType): Single;
begin
	Result := 1;
	case AmmoKind of
		Ballistic: Result := 1/BALLISTIC_FIRE_RATE;
		//Missile: Result := 1/MISSILE_FIRE_RATE;
	end;
end;

//Returns true if the timer has passed the threshhold value or if the timer has not yet been started
//TimerAllowsAction(Timer, Single): Boolean
function TimerAllowsAction(const Clock: Timer; Threshold: Single): Boolean;
begin
	if (GetTimerSeconds(Clock) > Threshold) or (TimerTicks(Clock) = 0) then
	begin
		result := true;
	end
	else Result := false;
end;

//Returns whether Invincible buff is active
//IsInvincible(BuffData): Boolean
function IsInvincible(const Powerup: BuffData): Boolean;
begin
	Result := Powerup.Invincible.Switch;
end;

//returns whether damage buff is active
//IsDamageBuff(BuffData): Boolean
function IsDamageBuff(const Powerup: BuffData): Boolean;
begin
	Result := Powerup.Damage.Switch;
end;

//returns whether speed buff is active
//IsSpeedBuff(BuffData): Boolean
function IsSpeedBuff(const Powerup: BuffData): Boolean;
begin
	Result := Powerup.Speed.Switch;
end;

//Returns a rotated vector by the specified angle in radians
//RotateVector(Vector, Single): Vector
function RotateVector(Vec: Vector; Theta: Single): Vector;
begin
	Result.x := Vec.x*Cos(Theta) - Vec.y*Sin(Theta);
	Result.y := Vec.y*Cos(Theta) + Vec.x*Sin(Theta);
end;

//multiplies the passed in vector by a number
//MultiplyVector(Vector, Single): Vector
function MultiplyVector(Vec: Vector; Num: Single): Vector;
begin
	Vec.x *= Num;
	Vec.y *= Num;
	Result := Vec;
end;

//Rotates the point by angle in radians around the passed in anchor of rotation
//RotatePoint(Point, Single, Single, Single)
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

//Rotates the passed in shape by theta(radians) around the passed in rotation anchor point
//RotateShape(LinesArray, Single, Single, Single)
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

///Moves a Point by the passed in vector
//MovePoint(Point2D, Vector)
procedure MovePoint(var Point: Point2D; const Vector: Vector);
begin
	Point.x += Vector.x;
	Point.y += Vector.y;
end;

//Moves a Line Segment by the passed in vector
//MoveLineSegment(LineSegment, Vector)
procedure MoveLineSegment(var Line: LineSegment; const Vector: Vector);
begin
	MovePoint(Line.StartPoint, Vector);
	MovePoint(Line.EndPoint, Vector);
end;

//Move an array of line segments by the passed in vector
//MoveShape(LinesArray, Vector)
procedure MoveShape(var Shape: LinesArray; const Vector: Vector);
var
	i: Integer;
begin
	for i:=0 to High(Shape) do
	begin
		MoveLineSegment(Shape[i], Vector);
	end;
end;

//teleports a shape to the passed in x, y coordinate
//TeleportShape(LinesArray, Single, Single, Point2D)
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

//Moves an array of Points by the passed in vector
//TeleportPointArray(Point2DArray, Single, Signle, Point2D)
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

//teleports ship to the specified x,y coord
//TeleportShip(ShipData, Single, Single)
procedure TeleportShip(var Ship: ShipData; const x, y: Single);
begin
	TeleportShape(Ship.Shape, x, y, Ship.Move.Pos);
	TeleportPointArray(Ship.AnchorPoint, x, y, Ship.Move.Pos);
	Ship.Move.Pos := PointAt(x, y);
end;

//Applies spin decay to a turn rate by Count times
//DecayRotation(Double, Integer)
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

//Applies speed decay to a vector by Count times
//DecayVelocity(Vector, Integer)
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

//returns whether there is a collision between LinesArrays
//CollisionBetweenShapes(LinesArray, LinesArray): Boolean
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

//Returns whether the player has space in their inventory
//InvHasSpace(InventoryArray): Boolean
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

//Returns the index of an empty inventory slot
//GetEmptyInvIndex(InventoryArray): Integer
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

//Returns whether an Item can be activated by the player or not
//CanActivateItem(ItemData, BuffData): Boolean
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

//Returns the bitmap of the specified item type
//GetItemBmp(ItemType): Bitmap
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

//Returns the max health of the passed in shipkind
//GetShipMaxHealth(ShipType): Integer
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

//Returns a random starting position that isn't in or near the wormhole
//GetStartingPosition(): Point2D
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

//Activates the named emitter/s
//ActivateEmittersNamed(array of emitter, String)
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

//creates a number popup record for drawing to screen
//SpawnNumberPopup(Single, PopupType, Single, Single, NumberPopupPtr);
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

//returns the type modifier of the passed in ship type
//GetTypeModifier(ShipType): Single
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

//Turns the timer package on
//StartTimerPackage(TimerPackage)
procedure StartTimerPackage(var Package: TimerPackage);
begin
	StartTimer(Package.Clock);
	StartTimer(Package.SubClock);
	Package.Switch := True;
end;

//Turns the timer package off
//StopTimerPackage(TimerPackage)
procedure StopTimerPackage(var Package: TimerPackage);
begin
	StopTimer(Package.Clock);
	StopTimer(Package.SubClock);
	Package.Switch := False;
end;

//frees the timers in the timer package
//ReleaseTimerPackage(TimerPackage)
procedure ReleaseTimerPackage(var Package: TimerPackage);
begin
	FreeTimer(Package.Clock);
	FreeTimer(Package.SubClock);
end;

//////////////////
// MainGame() Factory functions
//////////////////

//return default, template loot record attributes
//CreateLoot(ItemType): LootData
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

//spawns a random loot pickup at the specified coordinates
//SpawnRandomLoot(LootDataArray, LootDataArray, Point2D)
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

//Returns a vector representing the particles velocity
//GetEmittingParticleVector(Single, Vector) Vector
function GetEmittingParticleVector(const MaxVel: Single; const Heading: Vector): Vector;
begin
	Result.x := MaxVel * Heading.x * (Random(Round(MaxVel/2)) + (MaxVel/2));
	Result.y := MaxVel * Heading.y * (Random(Round(MaxVel/2)) + (MaxVel/2));
end;

//Builds and returns a Particle Entity
//CreateParticle(EmitterData): ParticleData
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

//returns ballistic type ammo
//CreateBallisticAmmo(): AmmoData
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

//Ammo Factory interface for creating and returning ammo instances
//CreateAmmo(AmmoType, Vector, Point2D, Single, Single, Owner): AmmoData
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

//loads the color set for ship collision flashing
//GetShipColors(LongWord): ShipColorArray
function GetShipColors(const BaseColor: LongWord): ShipColorArray;
begin
	Result[2] := ColorWhite;
	Result[3] := ColorRed;
	Result[1] := BaseColor;
	Result[0] := BaseColor;
end;

//Adds a tool to the Ship at the specified anchorpoint with the specified AmmoType
//AddTool(ShipData, Integer, AmmoType)
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

//Adds an emitter to the Ship at the specified anchorpoint with the specified attributes
//CreateEmitter(ShipData, Integer, String, Single, Single, Single, Single, Double, LongWord, LongWord);
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

//Setup Emitter Pointers to parent ship
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

//Setup Tool Pointers to parent ship
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

//reads the ships emitters from file
//ReadShipEmitters(ShipData, TextFile): ShipData
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

//reads the ships tools from file
//ReadShipTools(ShipData, TextFile): ShipData
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

//returns the ships anchorpoints from file
//ReadShipAnchorPoints(ShipData, TextFile): ShipData
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

//returns the ships extents from file
//ReadShipExtents(ShipData, TextFile): ShipData
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

//Returns the ships shape from file
//ReadShipShape(LinesArray, Point2D, TextFile): ShipData
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

//read in the ship from the file
//ReadLinesFromFile(ShipData, TextFile): ShipData
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

//initialise the textfile for reading in ship data and returns a Ship Entity
//GetShipFromFile(ShipData): ShipData
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

//returns a base movement model for an unowned ship
//GetBaseMovementModel(ShipType): MovementModel
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

//creates and returns a timer package (2 timers and a control switch)
//CreateTimerPackage(): TimerPackage;
function CreateTimerPackage(): TimerPackage;
begin
	Result.Clock := CreateTimer();
	Result.SubClock := CreateTimer();
	Result.Switch := False;
end;

//Apply difficulty modifier to the ships attributes and return a ship entity
//ApplyNPCModifiers(ShipData, DifficultyType): ShipData
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

//Constructs and returns a ship of the specified kind at the specified Pos x, y coords
//CreateShip(ShipType, LongWWord, OwnerType): ShipData
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

//////////////////
// MainGame() Check for Game End
//////////////////

//Checks and returns if the game has ended. Also has side effect of deciding whether the player won or loss.
//IsPlayerWin(Boolean, Boolean): Boolean
function IsPlayerWin(const Wormhole: WormholeData; const PlayerShip: ShipData): Boolean;
begin
	if not (Wormhole.IsAlive) and (PlayerShip.IsAlive) then
	begin
		Result := True;
	end
	else Result := False;
end;

//adds a delay on battle end and checks whether to quit the maingame() event loop or not
//BattleEndDelay(Timer, LongWord, Single, Boolean): Boolean
function BattleEndDelay(const EndDelay: Timer; const Delay: Single; const WormholeIsAlive: Boolean; const PlayerIsAlive: Boolean): Boolean;
begin
	Result := False;

	if not (WormholeIsAlive) or not (PlayerIsAlive) then
	begin
		if (GetTimerSeconds(EndDelay) = 0) then
		begin
			StartTimer(EndDelay);
		end
		else if (GetTimerSeconds(EndDelay) > Delay) then
		begin
			Result := True;
			StopTimer(EndDelay);
		end;
	end;
end;

//////////////////
// MainGame() Draw procedures
//////////////////

//Draws LinesArray Shape
//DrawShape(LongWord, LinesArray)
procedure DrawShape(const Shape: LinesArray; const Color: LongWord);
var
	i: Integer;
begin
	for i:=0 to High(Shape) do
	begin
		DrawLine(Color, Shape[i]);
	end;
end;

//draws a shape that randomly alternates between two specified colors
//DrawTwinklingShape(LinesArray, LongWord, LongWord)
procedure DrawTwinklingShape(const Shape: LinesArray; Color1, Color2: LongWord);
var
	Color: LongWord;
begin
	Color := Color1;
	case random(2) of 
		0: Color := Color1;
		1: Color := Color2;
	end;

	DrawShape(Shape, Color);
end;

//Draws buff icons on the UI when they are active
//DrawPlayerBuffs(BuffData, UIData);
procedure DrawPlayerBuffs(const Powerup: BuffData; const UI: UIData);
var
	TxtToDraw: String;
begin
	if IsDamageBuff(Powerup) then
	begin
		DrawBitmap(UI.DamageBuffIcon, WINDOW_WIDTH/2 - (BitmapWidth(UI.DamageBuffIcon)*2), BitmapHeight(UI.DamageBuffIcon)/2, OptionToScreen());
		TxtToDraw := FloatToStrF(BUFF_DURATION - GetTimerSeconds(Powerup.Damage.Clock), ffFixed, 8, 2);
		DrawText(TxtToDraw, ColorWhite, 'Arial.ttf', 12, WINDOW_WIDTH/2 - (BitmapWidth(UI.DamageBuffIcon)*1.8), BitmapHeight(UI.DamageBuffIcon) * 1.5 , OptionToScreen());
	end;

	if IsInvincible(Powerup) then
	begin
		DrawBitmap(UI.InvincibleBuffIcon, WINDOW_WIDTH/2 - (BitmapWidth(UI.InvincibleBuffIcon)/2), BitmapHeight(UI.InvincibleBuffIcon)/2, OptionToScreen());
		TxtToDraw := FloatToStrF(BUFF_DURATION - GetTimerSeconds(Powerup.Invincible.Clock), ffFixed, 8, 2);
		DrawText(TxtToDraw, ColorWhite, 'Arial.ttf', 12, WINDOW_WIDTH/2 - (BitmapWidth(UI.InvincibleBuffIcon)/2)*0.6, BitmapHeight(UI.InvincibleBuffIcon) * 1.5 , OptionToScreen());
	end;

	if IsSpeedBuff(Powerup) then
	begin
		DrawBitmap(UI.SpeedBuffIcon, WINDOW_WIDTH/2 + (BitmapWidth(UI.SpeedBuffIcon)), BitmapHeight(UI.SpeedBuffIcon)/2, OptionToScreen());
		TxtToDraw := FloatToStrF(BUFF_DURATION - GetTimerSeconds(Powerup.Speed.Clock), ffFixed, 8, 2);
		DrawText(TxtToDraw, ColorWhite, 'Arial.ttf', 12, WINDOW_WIDTH/2 + (BitmapWidth(UI.SpeedBuffIcon)*1.2), BitmapHeight(UI.SpeedBuffIcon)*1.5, OptionToScreen());
	end;
end;

//Draws the Inventory UI
//DrawInventory(InventoryArray; UIData)
procedure DrawInventory(const Inventory: InventoryArray; const UI: UIData);
var
	i, SlotIndex: Integer;
	x, y: Single;
begin
	SlotIndex := 0;
	for i:=0 to High(Inventory) do
	begin
		if not (Inventory[i].Drained) then
		begin
			x := WINDOW_WIDTH/2 - (BitmapWidth(UI.Inventory[i])*1.5) + SlotIndex*BitMapWidth(UI.Inventory[i]);
			y := WINDOW_HEIGHT - BitmapHeight(UI.Inventory[i]) - BitmapHeight(UI.HealthContainer) - 20; // 20 is padding between health container and inventory icons.
			DrawBitmap(UI.Inventory[i], x, y, OptionToScreen());
			SlotIndex += 1;
		end;
	end;
end;

//returns the maximum dps the array of tools can output
//ToolarrayMaxDps(array of ToolData): Single
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

procedure DrawSpawnTimer(const SpawnTimer: Timer; const WormholeAlive: Boolean; const Difficulty: DifficultyType);
var
	TxtToDraw: String;
begin
	if (WormholeAlive) then
	begin
		TxtToDraw := FloatToStrF((SPAWN_TIME / GetDifficultyModifier(Difficulty)) - GetTimerSeconds(SpawnTimer), ffFixed, 8, 2);
		DrawText(TxtToDraw, ColorRed, 'Arial.ttf', 12, -10, -6);
	end;
end;

//Draws a meter which tracks the percent of the screen covered by the wormhole
//DrawWormholeMeter(Bitmap, array of ToolData, WormholeData);
procedure DrawWormholeMeter(const WHMeterContainer: Bitmap; const Wormhole: WormholeData; const Difficulty: DifficultyType);
var
	x, y, w, h: Single;
	PlayArea, WormArea, MeterLevel: Single;
	WormRect: Rectangle;
begin
	//meter container bitmap
	w := BitMapWidth(WHMeterContainer);
	h := BitMapHeight(WHMeterContainer);
	x := WINDOW_WIDTH - w;
	y := WINDOW_HEIGHT - h;
	DrawBitmap(WHMeterContainer, x, y, OptionToScreen());

	PlayArea := PLAY_WIDTH * PLAY_HEIGHT;
	WormRect := RectangleFrom(Wormhole.Shape);
	WormArea := WormRect.width * WormRect.height;

	//Meter level
	if (PlayArea <> 0) and (WormArea < (PlayArea/2)) then
	begin
		MeterLevel := WormArea / (PlayArea/2);
		//MeterLevel := 0.5;
	end
	else
	begin
		MeterLevel := 1;
	end;

	//Draw Wormhole meter
	FillRectangle(Wormhole.Color[1], x+1, y+1 + (1-MeterLevel)*h, w - 2, MeterLevel * h-2, OptionToScreen());
end;

//Draws the health bar onto the UI
//DrawHealthBar(Boolean, Single, ShipType, Bitmap)
procedure DrawHealthBar(const IsAlive, Invincible: Boolean; const Health: Single; const ShipKind: ShipType; const Container: Bitmap);
var
	x, y, w, h, HealthPercent: Single;
	color: LongWord;
begin
	w := BitmapWidth(Container);
	h := BitmapHeight(Container);
	x := WINDOW_WIDTH/2 - w/2;
	y := WINDOW_HEIGHT - h - 10; //10 padding between healthbar and inventory bar
	DrawBitmap(Container, x, y, OptionToScreen());

	if (IsAlive) then
	begin
		case (Invincible) of
			True: color := ColorYellow;
			False: color := ColorRed;
			else color := ColorRed;
		end;

		//don't draw negative health
		if (Health > 0) then
		begin
			HealthPercent := Health / GetShipMaxHealth(ShipKind);
		end
		else HealthPercent := 0;

		FillRectangle(color, x+1, y+1, abs(HealthPercent*w-2), h-2, OptionToScreen());
	end;
end;

//draws wormhole related UI elements
//DrawWormholeUI(WormholeData, UIData, Timer, DifficultyType);
procedure DrawWormholeUI(const Wormhole: WormholeData; const UI: UIData; const SpawnTimer: Timer; const Difficulty: DifficultyType);
begin
	DrawWormholeMeter(UI.WHMeterContainer, Wormhole, Difficulty);
	DrawSpawnTimer(SpawnTimer, Wormhole.IsAlive, Difficulty);
end;

//SubController for drawing the UI
//DrawPlayerUI(ShipData, UIData, InventoryArray, Integer)
procedure DrawPlayerUI(const Ship: ShipData; const UI: UIData; const Inventory: InventoryArray; const Score: Integer);
begin
	DrawHealthBar(Ship.IsAlive, Ship.Powerup.Invincible.Switch, Ship.Health, Ship.ShipKind, UI.HealthContainer);
	DrawInventory(Inventory, UI);
	DrawPlayerBuffs(Ship.Powerup, UI);
	//DrawFramerate(10, 10);

	//score
	DrawText(FloatToStrF(Score, ffFixed, 4, 0), ColorGreen, 'Score', WINDOW_WIDTH - 75, 15, OptionToScreen());
end;

//draws all number popups to the screen
//DrawNumberPopups(array of NumberPopupData);
procedure DrawNumberPopups(const NumberPopups: array of NumberPopupData);
var
	i: Integer;
begin
	for i:=0 to High(NumberPopups) do
	begin
		DrawText(IntToStr(NumberPopups[i].Number), NumberPopups[i].Color, NumberPopups[i].Font, NumberPopups[i].Move.Pos.x, NumberPopups[i].Move.Pos.y);
	end;
end;

//Draws emitter particles
//DrawParticles(ParticleData, LongWord, LongWord)
procedure DrawParticle(const Particle: ParticleData; const Color1, Color2: LongWord);
begin
	if (Particle.IsAlive) then
	begin
		DrawTwinklingShape(Particle.Shape, Color1, Color2);
	end;
end;

//emitter level controller for drawing emitterdata and it's childdren elements
//DrawEmitter(EmitterData)
procedure DrawEmitter(const Emitter: EmitterData; const SpeedBuff: Boolean);
var
	i: Integer;
begin
	for i:=0 to High(Emitter.Particles) do
	begin
		if (Emitter.EmitterName = 'Thruster') and (SpeedBuff) then
		begin			
			DrawParticle(Emitter.Particles[i], ColorBlue, ColorSkyBlue);
		end
		else DrawParticle(Emitter.Particles[i], Emitter.Color[0], Emitter.Color[1]);
	end;
end;

procedure DrawEmitterArray(const Emitters: EmitterDataArray; const SpeedBuff: Boolean);
var
	i: Integer;
begin
	for i:=0 to High(Emitters) do
	begin
		DrawEmitter(Emitters[i], SpeedBuff);
	end;
end;

//Draw all ammo elements within the ammo list
//DrawAmmoList(AmmoListArray, Boolean)
procedure DrawAmmoList(const AmmoList: AmmoListArray; const DamageBuff: Boolean);
var
	i: Integer;
	color: LongWord;
begin
	for i:=0 to High(AmmoList) do
	begin
		if (AmmoList[i].IsAlive) then
		begin
			case (DamageBuff) of
				True: color := ColorRed;
				False: color := AmmoList[i].Color;
			end;

			DrawShape(AmmoList[i].Shape, Color);
		end;
	end;
end;

//Draws the tool shapes on the ship
//DrawTool(ToolData);
procedure DrawTool(const Tool: ToolData);
var
	i: Integer;
	AnchorPoint: Point2D;
	x1, y1, x2, y2: Single;
	color: LongWord;
begin
	AnchorPoint := Tool.Anchor^;
	for i:=0 to High(Tool.Shape) do
	begin
		x1 := Tool.Shape[i].StartPoint.x;
		y1 := Tool.Shape[i].StartPoint.y;
		x2 := Tool.Shape[i].EndPoint.x;
		y2 := Tool.Shape[i].EndPoint.y;

		case TimerAllowsAction(Tool.CoolDown.Clock, GetToolReloadTime(Tool.AmmoKind)) of
			True: color := Tool.Color[0];
			False: color := Tool.Color[1];
		end;

		DrawLine(color, x1 + AnchorPoint.x, y1 + AnchorPoint.y, x2 + AnchorPoint.x, y2 + AnchorPoint.y);
	end;

	//debug line
	//DrawLine(ColorGreen, Tool.Anchor^.x, Tool.Anchor^.y, Tool.Anchor^.x + (Tool.Heading^.x * 20), Tool.Anchor^.y + (Tool.Heading^.y * 20));
end;

procedure DrawToolArray(const Tools: ToolDataArray);
var
	i: Integer;
begin
	for i:=0 to High(Tools) do
	begin
		DrawTool(Tools[i]);
	end;
end;

//draws ammo that has been spawned by the tool
//DrawToolAmmo(ToolDataArray, TimerPackage)
procedure DrawToolAmmo(const Tools: ToolDataArray; const DamageBuff: Boolean);
var
	i: Integer;
begin
	for i:=0 to High(Tools) do
	begin
		DrawAmmoList(Tools[i].SpawnedAmmo, DamageBuff);
	end;
end;

//Ship level controller for drawing elements
//DrawShip(ShipData)
procedure DrawShip(const Ship: ShipData);
begin
	if (Ship.IsAlive) then
	begin
		//draw parent ship
		DrawShape(Ship.Shape, Ship.Color[0]);

		//draw tools
		DrawToolArray(Ship.Tool);

		//draw emitters
		DrawEmitterArray(Ship.Emitter, IsSpeedBuff(Ship.Powerup));

		//draw ammo
		DrawToolAmmo(Ship.Tool, IsDamageBuff(Ship.Powerup));
	end;	
end;

//Draw the starry background
//DrawStars(StarArray)
procedure DrawStars(const Stars: StarArray);
var
	i: Integer;
	color: LongWord;
begin
	if (High(Stars)<0) then
	begin
		WriteLn('DrawStars() No Stars to Draw');
	end
	else
		begin
		//get the star color
		color := ColorWhite;
		for i:=0 to High(Stars) do
		begin
			if (Stars[i].Size > BASE_STAR_SIZE) then
			begin
				case Random(4) of
					0: color := ColorLightSteelBlue;
					1: color := ColorAquamarine;
					2: color := ColorSkyBlue;
					3: color := ColorCyan;
				end;
			end
			else
			begin
				case Random(3) of
					0: color := ColorWhite;
					1: color := ColorGhostWhite;
					2: color := ColorLightYellow;
				end;
			end;
			//Draw the star
			FillRectangle(color, (Stars[i].Pos.x - Stars[i].Size/2), (Stars[i].Pos.y - Stars[i].Size/2), Stars[i].Size, Stars[i].Size);
		end;
	end;
end;

//Draws the wormhole
//DrawWormhole(array of LongWord, Circle, Boolean)
procedure DrawWormhole(const Color: array of LongWord; const Shape: Circle; const IsAlive: Boolean);
begin
	if IsAlive then
	begin
		FillCircle(Color[0], Shape);
		DrawCircle(Color[1], Shape);
	end;
end;

//handler for drawing background data
//DrawBackground(BackgroundData)
procedure DrawBackground(const Background: BackgroundData);
begin
	DrawStars(Background.Stars);
end;

procedure DrawDebrisList(const DebrisList: DebrisListArray);
var
	i: Integer;
begin
	for i:=0 to High(DebrisList) do
	begin
		DrawShape(DebrisList[i].Shape, DebrisList[i].Color);
	end;
end;

procedure DrawLootList(const LootList: LootDataArray);
var
	i: Integer;
begin
	for i:=0 to High(LootList) do
	begin
		if (Length(LootList[i].Color) = 2) and not (LootList[i].PickedUp) then
		begin
			DrawTwinklingShape(LootList[i].Shape, LootList[i].Color[0], LootList[i].Color[1]);
		end;
	end;
end;

//Draws level elements
//DrawLevel(Level)
procedure DrawLevel(const LevelData: Level);
begin
	DrawBackground(LevelData.Background);
	DrawWormhole(LevelData.Wormhole.Color, LevelData.Wormhole.Shape, LevelData.Wormhole.IsAlive);

	DrawDebrisList(LevelData.DebrisList);

	DrawLootList(LevelData.LootList);
end;

procedure DebugShip(const Ship: ShipData);
var
	i: Integer;
begin
	//writeln(' ship Pos x: ', Ship.Move.Pos.x, ' Ship Pos y: ', Ship.Move.Pos.y);

	FillRectangle(ColorYellow, Ship.Move.Pos.x, Ship.Move.Pos.y, 10, 10);
	for i:=0 to High(Ship.AnchorPoint) do
	begin
		FillRectangle(ColorRed, Ship.AnchorPoint[i].x, Ship.AnchorPoint[i].y, 10, 10);
	end;
end;

procedure DrawNPCTierShips(const NPCShips: ShipDataArray);
var
	i: Integer;
begin	
	for i:=0 to High(NPCShips) do
	begin
		DrawShip(NPCShips[i]);

		//DEBUG
		//DebugShip(NPCShips[i]);
	end;
end;

//Main Controller for drawing game elements
//DrawGame(Game, DifficultyType)
procedure DrawGame(const PlayerData: Player; const NPCData: NPCTierArray; const LevelData: Level; const Difficulty: DifficultyType);
var
	i: Integer;
begin
	DrawLevel(LevelData);
	DrawShip(PlayerData.Ship);
	DrawNumberPopups(PlayerData.NumberPopups);

	for i:=0 to High(NPCData) do
	begin
		DrawNPCTierShips(NPCData[i].Ships);
	end;
	
	DrawPlayerUI(PlayerData.Ship, PlayerData.UI, PlayerData.Inventory, LevelData.Score);	
	DrawWormholeUI(LevelData.Wormhole, PlayerData.UI, LevelData.SpawnTimer, Difficulty);

	//
	//DEBUG
	//DebugShip(PlayerData.Ship);
end;

//////////////////
// MainGame() Update Position procedures
//////////////////

//Update the position of the ship, its children line segments, and any anchor points based on its Velocity vector
//ApplyVelocity(MovementModel, LinesArray, Boolean)
procedure ApplyVelocity(var Move: MovementModel; var Shape: LinesArray; SpeedBuff: Boolean = False);
begin
	if (High(Shape)>=0) or (abs(Move.Vel.x) + abs(Move.Vel.y) > 0) then
	begin
		//move ship's central reference pposition and its shape
		MovePoint(Move.Pos, MultiplyVector(Move.Vel, GetSpeedBuffMod(SpeedBuff)));
		MoveShape(Shape, MultiplyVector(Move.Vel, GetSpeedBuffMod(SpeedBuff)));
	end
end;

//Clamps Velocity to it's Max Velocity in any direction
//ClampVelocity(Ship, Boolean)
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

//controller for updating ship object positions
//UpdateEntityPos(ShipData)
procedure UpdateEntityPos(var Move: MovementModel; var Shape: LinesArray; const SpeedBuff: Boolean = False);
begin
	ClampVelocity(Move, SpeedBuff);
	ApplyVelocity(Move, Shape, SpeedBuff);
end;

//iterates through all the ammo created by the tool to update them
//UpdateTool(ToolData)
procedure UpdateTool(var Tool: ToolData);
var
	i: Integer;
begin
	//for i:=0 to High(Tool.Shape) do
	//begin
		//writeln(' cx1: ', Tool.Shape[i].StartPoint.x, ' cy1: ', Tool.Shape[i].StartPoint.y, ' cx2: ', Tool.Shape[i].EndPoint.x, ' cy2: ', Tool.Shape[i].EndPoint.y)
	//end;

	//update tool ammo
	for i:=0 to High(Tool.SpawnedAmmo) do
	begin
		UpdateEntityPos(Tool.SpawnedAmmo[i].Move, Tool.SpawnedAmmo[i].Shape);
	end;
end;

//Updates the position of Emitter Particles
//UpdateParticle(ParticleData)
procedure UpdateParticle(var Particle: ParticleData);
begin
	//Move the Particle
	MovePoint(Particle.Move.Pos, Particle.Move.Vel);
	MoveShape(Particle.Shape, Particle.Move.Vel);

	//rotate the Particle
	RotateShape(Particle.Shape, Particle.Move.TurnRate, Particle.Move.Pos.x, Particle.Move.Pos.y);

	//Apply Decay to the particle
	DecayRotation(Particle.Move.TurnRate, 2);
	DecayVelocity(Particle.Move.Vel, 4);
end;

//Updates the state of the emitter and it's children elements
//UpdateEmitter(EmitterData)
procedure UpdateEmitter(var Emitter: EmitterData);
var
	i: Integer;
begin
	for i:=0 to High(Emitter.Particles) do
	begin
		UpdateParticle(Emitter.Particles[i]);
	end;
end;

//Updates Number Popup screen positions
//UpdateNumberPopups(NumberPopupArray)
procedure UpdateNumberPopups(var NumberPopups: NumberPopupArray);
var
	i: Integer;
begin
	if (Length(NumberPopups) > 0) then
	begin
		for i:=0 to High(NumberPopups) do
		begin
			MovePoint(NumberPopups[i].Move.Pos, NumberPopups[i].Move.Vel);
		end;
	end;
end;

//Updates the inventory slot bitmaps for drawing
//UpdateInventoryBmp(InventoryArray, UIData);
procedure UpdateInventoryBmp(var Inventory: InventoryArray; var UI: UIData);
var
	i, InvSlot: Integer;
begin
	//count to keep track of the inventory slot to load the bitmap into
	InvSlot:=0;
	for i:=0 to High(Inventory) do
	begin
		if not (Inventory[i].Drained) then
		begin
			UI.Inventory[InvSlot] := GetItemBmp(Inventory[i].ItemKind);
			InvSlot += 1;
		end
	end;
end;

//Flash the Ships colors based on a timer
//FlashInvincible(array of LongWord, Timer, Boolean): Boolean
function FlashInvincible(var Color: array of LongWord; const SubClock: Timer; const Invincible: Boolean): Boolean;
begin
	// if the timer hits threshhold then we flash colors
	if (Invincible) and (GetTimerSeconds(SubClock) > 1/COLOR_FLASH_RATE/2) then
	begin
		case random(8) of
			1: Color[0] := ColorRed;
			2: Color[0] := ColorOrange;
			3: Color[0] := ColorYellow;
			4: Color[0] := ColorGreen;
			5: Color[0] := ColorBlue;
			6: Color[0] := ColorIndigo;
			7: Color[0] := ColorViolet;
		end;
		
		ResetTimer(SubClock);
		Result := True;
	end
	else if not (Invincible) then
	begin
		StopTimer(SubClock);
		Result := False;
	end;
end;

//Flashes the ships colors
//Flash(Timer, Boolean, array of LongWord)
procedure Flash(const SubClock: Timer; var Switch: Boolean; var Color: array of LongWord);
begin
	//color switching
	if GetTimerSeconds(SubClock) > 1/COLOR_FLASH_RATE then
	begin
		case Switch of 
			True:
			begin
				Switch := False;
				Color[0] := Color[2];
			end;
			False: 
			begin
				Switch := True;
				Color[0] := Color[3];
			end;
		end;
		ResetTimer(SubClock);
	end;
end;

//Manages collision timer behaviour for color flashing. Returns whether it flashed the ships color or not
//CollisionFlash(TimerPackage, array of LongWord)
function CollisionFlash(var TimerPackage: TimerPackage; var ShipColor: array of LongWord): Boolean;
begin
	if (GetTimerSeconds(TimerPackage.Clock) > COLLIDE_TIME_OUT) then
	begin
		StopTimerPackage(TimerPackage);
		result := False;
	end
	else if (GetTimerSeconds(TimerPackage.Clock) > 0) then
	begin
		Flash(TimerPackage.SubClock, TimerPackage.Switch, ShipColor);
		result := True;
	end
	else result := False;
end;

//Update the color of the ship based on any collision or hit timers
//UpdateShipColor(array of LongWord, TimerPackage, TimerPackage, TimerPackage);
procedure UpdateShipColor(var ShipColor: array of LongWord; var ShipCollide: TimerPackage; var HitByAmmo: TimerPackage; const Invincible: TimerPackage);
begin
	//invincible star power flashing
	if (FlashInvincible(ShipColor, Invincible.SubClock, Invincible.Switch)) then
	begin
		Exit;
	end
	//ship collision flashing
	else if (CollisionFlash(ShipCollide, ShipColor)) then
	begin
		Exit;
	end
	//ammo collision flashing
	else if (CollisionFlash(HitByAmmo, ShipColor)) then
	begin
		Exit;
	end
	else ShipColor[0] := ShipColor[1]; //reset ship color if not flashing
end;

//Rotates Ship instance
//RotateShip(ShipData, Boolean);
procedure RotateShip(var Ship: ShipData; SpeedBuff: Boolean = False);
var
	i: Integer;
	theta: Double;
	RotationAnchor: Point2D;
begin
	if (VectorsNotEqual(Ship.Move.Heading, Ship.Move.TargetHeading)) then
	begin
		//calculate the angle to rotate by
		theta := CalculateAngle(Ship.Move.Heading, Ship.Move.TargetHeading) * (PI/180);
		theta := ClampNumber(theta, Ship.Move.TurnRate * GetSpeedBuffMod(SpeedBuff)); //Make sure we are not turning more than allowed
		Ship.Move.Heading := RotateVector(Ship.Move.Heading, theta);

		//apply rotation to the ship and its children elements
		RotationAnchor := Ship.Move.Pos;
		RotateShape(Ship.Shape, theta, RotationAnchor.x, RotationAnchor.y);

		//rotate tools and anchorpoints
		for i:=0 to High(Ship.Tool) do
		begin
			RotateShape(Ship.Tool[i].Shape, theta);
		end;
		for i:=0 to High(Ship.AnchorPoint) do
		begin
			RotatePoint(Ship.AnchorPoint[i], Theta, RotationAnchor.x, RotationAnchor.y);
		end;		
	end;
end;

//apply friction to the ship if no player input detected.
//ApplyFriction(MovementModel)
procedure ApplyFriction(var Move: MovementModel);
begin
	if not KeyDown(WKey) and not KeyDown(SKey) and not KeyDown(QKey) and not KeyDown(EKey) then
	begin
		Move.Vel := MultiplyVector(Move.Vel, BASE_FRICTION);
	end;
end;

//updates the position of anchorpoints
//UpdateAnchorPoints(Point2DArray, Vector, TimerPackage)
procedure UpdateAnchorPoints(var AnchorPoint: Point2DArray; const Move: MovementModel; const SpeedBuff: Boolean);
var
	i: Integer;
begin
	for i:=0 to High(AnchorPoint) do
	begin
		MovePoint(AnchorPoint[i], MultiplyVector(Move.Vel, GetSpeedBuffMod(SpeedBuff)));
	end;
end;

//Updates the position and rotation of loot entities
//RotateLoot(LootData)
procedure RotateLoot(var Loot: LootData);
begin
	RotateShape(Loot.Shape, Loot.Move.TurnRate, Loot.Move.Pos.x, Loot.Move.Pos.y);
end;

//Updates the position and rotation of debris entities
//UpdateDebris(Debris)
procedure UpdateDebris(var Debris: Debris);
begin
	ApplyVelocity(Debris.Move, Debris.Shape);
	ClampVelocity(Debris.Move);
	RotateShape(Debris.Shape, Debris.Move.TurnRate, Debris.Move.Pos.x, Debris.Move.Pos.y);
	DecayRotation(Debris.Move.TurnRate);
	DecayVelocity(Debris.Move.Vel);
end;

//Sub controller for updating level entities
//UpdateLevel(Level, DifficultyType)
procedure Updatelevel(var LevelData: Level; const Difficulty: DifficultyType);
var
	i: Integer;
begin
	for i:=0 to High(LevelData.DebrisList) do
	begin
		UpdateDebris(LevelData.DebrisList[i])
	end;

	for i:=0 to High(LevelData.LootList) do
	begin
		RotateLoot(LevelData.LootList[i]);
	end;
end;

//Parent handler for updating the ship instance
//UpdateShip(ShipData)
procedure UpdateShip(var Ship: ShipData);
var
	i: Integer;
begin
	if Ship.IsAlive then
	begin
		//only apply friction for player
		if (Ship.owner = HumanPlayer) then
		begin
			ApplyFriction(Ship.Move);
		end;

		UpdateEntityPos(Ship.Move, Ship.Shape, IsSpeedBuff(Ship.Powerup));
		UpdateAnchorPoints(Ship.AnchorPoint, Ship.Move, IsSpeedBuff(Ship.Powerup));
		RotateShip(Ship);
		UpdateShipColor(Ship.Color, Ship.ShipCollide, Ship.HitByAmmo, Ship.Powerup.Invincible);

		//update children Tool and ammo instances
		for i:=0 to High(Ship.Emitter) do
		begin
			UpdateEmitter(Ship.Emitter[i]);
		end;
		for i:=0 to High(Ship.Tool) do
		begin
			UpdateTool(Ship.Tool[i]);
		end;
	end;
end;

//Sub controller for updating NPC Instances
//UpdateNPCTierShips(NPCTier)
procedure UpdateNPCTierShips(var NPCTier: NPCTier);
var
	i: Integer;
begin
	for i:=0 to High(NPCTier.Ships) do
	begin
		UpdateShip(NPCTier.Ships[i]);
	end;
end;

//sub controller for updating the player
//UpdatePlayer(Player)
procedure UpdatePlayer(var PlayerData: Player);
begin
	UpdateShip(PlayerData.Ship);
	UpdateNumberPopups(PlayerData.NumberPopups);
end;

//Main Controller for updating the game state
//UpdateGamePositions(Game, DifficultyType)
procedure UpdateGamePositions(var GameModule: Game; const Difficulty: DifficultyType);
var
	i: Integer;
begin
	UpdatePlayer(GameModule.PlayerData);

	for i:=0 to High(GameModule.NPCData) do
	begin
		UpdateNPCTierShips(GameModule.NPCData[i]);
	end;

	UpdateLevel(GameModule.LevelData, Difficulty);
end;

//////////////////
// MainGame() Handle Game State Procedures
//////////////////

procedure ReleaseShipTimers(var Ship: ShipData);
begin
	//timers
	ReleaseTimerPackage(Ship.Powerup.Invincible);
	ReleaseTimerPackage(Ship.Powerup.Damage);
	ReleaseTimerPackage(Ship.Powerup.Speed);
	ReleaseTimerPackage(Ship.ShipCollide);
	ReleaseTimerPackage(Ship.WormholeCollide);
	ReleaseTimerPackage(Ship.HitByAmmo);
end;

//Iterate backwards through the array to remove used items from the inventory
//RemoveDead(InventoryArray)
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

//Iterate backwards through the array to remove expired number popups
//RemoveDead(NumberPopupArray);
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

//Iterate backwards through the array to remove picked up loot
//RemoveDead(LootDataArray);
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

//Iterate backwards through the array to remove expired ammo
//RemoveDead(AmmoListArray);
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

//Iterate backwards through the array to remove expired particles
//RemoveDead(ParticleDataArray);
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

//Iterate backwards through the array to remove expired particles
//RemoveDead(DebrisListarray);
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

//AssertShipPointers(ShipDataArray, Integer)
procedure AssertShipPointers(var Ship: ShipData);
var
	NumOfAnchors: Integer;
begin
	NumOfAnchors := Length(Ship.AnchorPoint);
	SetupToolPointers(Ship.Tool, NumOfAnchors, Ship.AnchorPoint, Ship.Move.Heading);
	SetupEmitterPointers(Ship.Emitter, NumOfAnchors, Ship.AnchorPoint, Ship.Move.Heading);
end;

//Iterate backwards through the array to remove dead Ships
//RemoveDead(ShipDataArray);
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

//Creates a particle instance
//CreateParticle(EmitterData)
procedure AddParticle(var Particles: ParticleDataArray; const EmitterAtrib: EmitterData);
var
	f: Integer;
begin
	SetLength(Particles, Length(Particles)+1);
	f := High(Particles);

	Particles[f] := CreateParticle(EmitterAtrib); //we pass the emitter record in to get the particle attributes specific to the emitter
end;

//Flags particle for destruction if it is expired
//ExpireParticle(Boolean, Timer, Single)
procedure ExpireParticle(var ParticleIsAlive: Boolean; const ParticleExpiryTimer: Timer; const Expiry: Single);
begin
	if (GetTimerSeconds(ParticleExpiryTimer) > Expiry) then
	begin
		ParticleIsAlive := False;
		StopTimer(ParticleExpiryTimer);
	end;
end;

//Manages the behaviour of the passed in array of emitters
//HandleEmitters(array of EmitterData)
procedure HandleEmitters(var Emitter: array of EmitterData; const Heading: HeadingPtr);
var
	i, j: Integer;
begin
	for i:=0 to High(Emitter) do
	begin
		//guard conditional
		if Emitter[i].EmissionRate <= 0 then
		begin
			Exit;
		end

		//spawning particles
		else if (Emitter[i].CoolDown.Switch) and TimerAllowsAction(Emitter[i].CoolDown.Clock, 1/Emitter[i].EmissionRate) then
		begin
			Emitter[i].Heading := Heading; //make sure heading pointer is properly set before we create a particle
			AddParticle(Emitter[i].Particles, Emitter[i]);

			StopTimer(Emitter[i].CoolDown.Clock);
			Emitter[i].CoolDown.Switch := False;
		end;

		//flag expired particles for destruction
		for j:=0 to High(Emitter[i].Particles) do
		begin
			ExpireParticle(Emitter[i].Particles[j].IsAlive, Emitter[i].Particles[j].Expiry, Emitter[i].Expiry);
		end;
	end;
end;

//Collide Ammo with Wormhole
//AmmoWormholeCollide(AmmoData, WormholeData, NumberPopupPtr, Boolean)
procedure AmmoWormholecollide(var Ammo: AmmoData; var Wormhole: WormholeData; const NumberPopups: NumberPopupPtr; const DamageBuff: Boolean = False);
begin
	Wormhole.Health -= Ammo.Damage * GetDamageBuffModifier(DamageBuff);
	SpawnNumberPopup(Ammo.Damage * GetDamageBuffModifier(DamageBuff), DamagePopup, Ammo.Move.Pos, NumberPopups);
	Ammo.IsAlive := False;
end;

//collide Ammo with Ship
//AmmoShipCollide(AmmoData, ShipData, NumberPopupPtr, Boolean)
procedure AmmoShipCollide(var Ammo: AmmoData; var Ship: ShipData; const NumberPopups: NumberPopupPtr; const DamageBuff: Boolean = False);
begin
	if not IsInvincible(Ship.Powerup) then
	begin
		StartTimer(Ship.HitByAmmo.Clock);
		StartTimer(Ship.HitByAmmo.SubClock);
		Ship.Health -= Ammo.Damage * GetDamageBuffModifier(DamageBuff);
		SpawnNumberPopup(Ammo.Damage * GetDamageBuffModifier(DamageBuff), DamagePopup, Ammo.Move.Pos, NumberPopups);
	end;
	Ammo.IsAlive := False;
end;

//Collides two Ships
//ShipShipCollide(ShipData, ShipData)
procedure ShipShipCollide(var PlayerShip: ShipData; var NPCShip: ShipData; const NumberPopups: NumberPopupPtr);
begin
	if TimerAllowsAction(PlayerShip.ShipCollide.Clock, COLLIDE_TIME_OUT) then
	begin
		if not IsInvincible(PlayerShip.Powerup) then
		begin
			StartTimer(PlayerShip.ShipCollide.Clock);
			StartTimer(PlayerShip.ShipCollide.SubClock);
			PlayerShip.Health -= 1;
			SpawnNumberPopup(1, DamagePopup, PlayerShip.Move.Pos, NumberPopups);

			PlayerShip.Move.Vel.x += NPCShip.Move.Vel.x * 1.5 + GetSign(NPCShip.Move.Vel.x)*1;
			PlayerShip.Move.Vel.y += NPCShip.Move.Vel.y * 1.5 + GetSign(NPCShip.Move.Vel.y)*1;
		end;

		NPCShip.Health -= 1;
		SpawnNumberPopup(1, DamagePopup, NPCShip.Move.Pos, NumberPopups);
	end;	
	
	//npc ships will always bounce off irregardless of any collision timeout
	NPCShip.Move.Vel.x -= NPCShip.Move.Vel.x * 1;
	NPCShip.Move.Vel.y -= NPCShip.Move.Vel.y * 1;
end;

//Collides the ship with the wormhole
//ShipWormholeCollide(Single, MovementModel, TimerPackage, BuffData, NumberPopupPtr)
procedure ShipWormholeCollide(var Ship: ShipData; const NumberPopups: NumberPopupPtr);
begin
	if TimerAllowsAction(Ship.WormholeCollide.Clock, COLLIDE_TIME_OUT) then
	begin
		if not IsInvincible(Ship.Powerup) then
		begin
			StartTimer(Ship.WormholeCollide.Clock);
			StartTimer(Ship.WormholeCollide.SubClock);
			Ship.Health -= 1;
			SpawnNumberPopup(1, DamagePopup, Ship.Move.Pos, NumberPopups);
		end;

		Ship.Move.Vel.x *= Ship.Move.Heading.x * 2;
		Ship.Move.Vel.y *= Ship.Move.Heading.y * 2;
	end;
end;

//PlayerShip - Wormhole collision
//CheckCollision(ShipData, WormholeData): Boolean
function CheckCollision(const Ship: ShipData; const Wormhole: WormholeData): Boolean; Overload;
var
	i: Integer;
begin
	Result := False;
	if (Ship.IsAlive) then
	begin
		for i:=0 to high(Ship.Shape) do
		begin
			if LineIntersectsCircle(Ship.Shape[i], Wormhole.Shape)  then
			begin
				Result := True;
				Exit;
			end;
		end;
	end;
end;

//Ammo - Wormhole Colision
//CheckCollision(AmmoData, WormholeData): Boolean
function CheckCollision(const Ammo: AmmoData; const Wormhole: WormholeData): Boolean; Overload;
var
	i: Integer;
begin
	Result := False;
	if (Ammo.Owner <> Wormhole.Owner) and (Ammo.IsAlive) then
	begin
		for i:=0 to High(Ammo.Shape) do
		begin
			if LineIntersectsCircle(Ammo.Shape[i], Wormhole.Shape) then
			begin
				Result := True;
				Exit;
			end;
		end;
	end;
end;

//Ship - Ship collision
//CheckCollision(ShipData, ShipData): Boolean
function CheckCollision(const Ship1: ShipData; const Ship2: ShipData): Boolean; Overload;
begin
	Result := False;
	if (Ship1.IsAlive) and (Ship2.IsAlive) then
	begin
		//range check first for optimisation
		if (PointPointDistance(Ship1.Move.Pos, Ship2.Move.Pos) < Ship1.Extents.w * 2) then
		begin
			if (CollisionBetweenShapes(Ship1.Shape, Ship2.Shape)) then
			begin
				Result := True;
				Exit;
			end;
		end;
	end;
end;

//Collision between Ammo and Ship
//CheckCollision(AmmoData, ShipData): Boolean
function CheckCollision(const Ammo: AmmoData; const Ship: ShipData): Boolean; Overload;
begin
	Result := False;
	if (Ammo.IsAlive) and (Ship.IsAlive) and (Ammo.Owner <> Ship.Owner) then
	begin
	 	if (PointPointDistance(Ammo.Move.Pos, Ship.Move.Pos) < Ship.Extents.w * 2) then
	 	begin
	 		if (CollisionBetweenShapes(Ammo.Shape, Ship.Shape)) then
			begin
				Result := True;
				Exit;
			end;
		end;
	end;
end;

//handles ammo - ship collisions
//AmmoCollisions(AmmoListArray, ShipData, NumberPopupPtr)
procedure AmmoListCollisions(var AmmoList: AmmoListArray; var Ship: ShipData; const NumberPopups: NumberPopupPtr; DamageBuff: Boolean = False);
var
	i: Integer;
begin
	for i:=0 to High(AmmoList) do
	begin
		if CheckCollision(AmmoList[i], Ship) then
		begin
			AmmoShipCollide(AmmoList[i], Ship, NumberPopups, DamageBuff)
		end;
	end;
end;

//handles collisions on the npc ship level
//ShipCollisions(ShipData, ShipData, NumberPopupPtr)
procedure ShipCollisions(var NPCShip: ShipData; var PlayerShip: ShipData; const NumberPopups: NumberPopupPtr);
var
	i: Integer;
begin
	//NPCShip - PlayerShip
	if CheckCollision(PlayerShip, NPCShip) then
	begin
		ShipShipCollide(PlayerShip, NPCShip, NumberPopups);
	end;

	//NPCAmmo - PlayerShip
	for i:=0 to High(NPCShip.Tool) do
	begin
		AmmoListCollisions(NPCShip.Tool[i].SpawnedAmmo, PlayerShip, NumberPopups);
	end;

	//PlayerAmmo - NPCShip
	for i:=0 to High(PlayerShip.Tool) do
	begin
		AmmoListCollisions(PlayerShip.Tool[i].SpawnedAmmo, NPCShip, NumberPopups, IsDamageBuff(PlayerShip.Powerup));
	end;
end;

//Sub handler for collisions involving NPC entities
//NPCTierCollisions(NPCTier, Player)
procedure NPCTierCollisions(var NPCShips: ShipDataArray; var PlayerShip: ShipData; const NumberPopups: NumberPopupPtr);
var
	i: Integer;
begin
	for i:=0 to High(NPCShips) do
	begin
		ShipCollisions(NPCShips[i], PlayerShip, NumberPopups);
	end;
end;

procedure AmmoWormholeCollisions(var AmmoList: AmmoListArray; var Wormhole: WormholeData; const NumberPopups: NumberPopupPtr; const DamageBuff: Boolean);
var
	i: Integer;
begin
	for i:=0 to High(AmmoList) do
	begin
		if CheckCollision(AmmoList[i], Wormhole) then
		begin
			AmmoWormholeCollide(AmmoList[i], Wormhole, NumberPopups, DamageBuff)
		end;
	end;
end;

//sub controller for collisions involving the wormhole
procedure WormholeCollisions(var Ship: ShipData; var Wormhole: WormholeData; const NumberPopups: NumberPopupPtr);
var
	i: Integer;
begin
	//PlayerShip - Wormhole collisions
	if CheckCollision(Ship, Wormhole) then
	begin
		ShipWormholeCollide(Ship, NumberPopups);
	end;

	//PlayerAmmo - Wormhole collisions
	for i:=0 to High(Ship.Tool) do
	begin
		AmmoWormholeCollisions(Ship.Tool[i].SpawnedAmmo, Wormhole, NumberPopups, IsDamageBuff(Ship.Powerup));
	end;
end;

//Parent handler for entity collisions
//HandleCollisionns(Player, WormmholeData, NPCTierArray)
procedure HandleCollisions(var PlayerData: Player; var Wormhole: WormholeData; var NPCData: NPCTierArray);
var
	i: Integer;
begin
	//NPC - Player collisions
	if (PlayerData.Ship.IsAlive) then
	begin
		for i:=0 to High(NPCData) do
		begin
			NPCTierCollisions(NPCData[i].Ships, PlayerData.Ship, @PlayerData.NumberPopups);
		end;

		WormholeCollisions(PlayerData.Ship, Wormhole, @PlayerData.NumberPopups);
	end;
end;

//Fires a tool weapon to create a new ammo record
//FireTool(ToolData, DifficultyType, OwnerType)
procedure FireTool(var Tool: ToolData; const Difficulty: DifficultyType; const Owner: OwnerType);
var
	f: Integer;
	DifficultyMod: Single = 1;
begin
	if TimerAllowsAction(Tool.CoolDown.Clock, GetToolReloadTime(Tool.AmmoKind)) then
	begin
		//start tool weapon cooldown
		StartTimer(Tool.CoolDown.Clock);

		//get difficulty modifier to apply to ammo values
		case owner of
			HumanPlayer: DifficultyMod := 1;
			Computer: DifficultyMod := GetDifficultyModifier(Difficulty);
		end;

		//get a new ammo record from the ammo factory
		SetLength(Tool.SpawnedAmmo, Length(Tool.SpawnedAmmo)+1);
		f := High(Tool.SpawnedAmmo);
		Tool.SpawnedAmmo[f] := CreateAmmo(Tool.AmmoKind, Tool.Anchor^, Tool.Heading^, Owner, DifficultyMod);		
	end;
end;

//Adds acceleration to the NPC Ship in the direction of its heading
//ThrustNPCShip(MovementModel, array of EmitterData)
procedure ThrustNPCShip(var Move: MovementModel; var Emitter: array of EmitterData);
var
	AngleToStartThrust: Single;
begin
	//only accelerate if pointing in the roughly the direction it wants to go
	AngleToStartThrust := 20;
	if (CalculateAngle(Move.TargetHeading, Move.Heading) < AngleToStartThrust) then
	begin
		Move.Vel.x += Move.Accel * Move.Heading.x;
		Move.Vel.y += Move.Accel * Move.Heading.y;
		ActivateEmittersNamed(Emitter, 'Thruster');
	end;
end;

//NPC randomly moves around the map
//ErraticBehaviour(MovementModel)
procedure ErraticBehaviour(var Move: MovementModel);
begin
	//set new random heading every 3 seconds on average
	case random(FPS*3) of
		((FPS*3)-1): 
		begin
			Move.TargetHeading := GetRandomVector(1,1);
		end;
	end;
end;

//NPC chases after the plaayer
//ChaseBehaviour(MovementModel, Point2D)
procedure ChaseBehaviour(var Move: MovementModel; const TargetPos: Point2D);
var
	CurrentVec, DesiredVec, SteeringVec: Vector;	
begin
	if (PointPointDistance(Move.Pos, TargetPos) <= AGRO_RANGE) then
	begin
		CurrentVec := Move.Vel;

		//get desired vector to player with magnitude of max velocity
		DesiredVec := VectorTo(TargetPos.x - Move.Pos.x, TargetPos.y - Move.Pos.y);
		DesiredVec := LimitVector(DesiredVec, Move.MaxVel);

		//get steering vector for counter acceleration
		SteeringVec := SubtractVectors(DesiredVec, CurrentVec);
		Move.TargetHeading := UnitVector(SteeringVec);
	end
	else ErraticBehaviour(Move);
end;

//Handles NPC tool firing behaviour
//FireNPCShipTools(ToolData, DifficultyType)
procedure FireNPCShipTools(var Tools: ToolDataArray; const Difficulty: DifficultyType);
var
	i, triggerNum: Integer;
begin
	for i:=0 to High(Tools) do
	begin
		//randomly fire tool weapon
		triggerNum := Round(FPS/NPC_FIRE_RATE) - 1;

		if (random(triggerNum+1) = triggerNum) then
		begin
			Tools[i].CoolDown.Switch := True;
		end;
	end;
end;

//Manages NPC Behaviour
//HandleNPCBehaviour(ShipData, Point2D);
procedure HandleNPCBehaviour(var Ship: ShipData; const TargetPos: Point2D; const Difficulty: DifficultyType);
begin
	if Ship.IsAlive then
	begin
		//movement
		case (Ship.NPCBehaviour) of
			Static: ; //no special behaviour if static
			Erratic: ErraticBehaviour(Ship.Move);
			Chase: ChaseBehaviour(Ship.Move, TargetPos);
		end;
		ThrustNPCShip(Ship.Move, Ship.Emitter);

		//shooting
		FireNPCShipTools(Ship.Tool, Difficulty);
	end;
end;

//Manages buff timers
//HandleBuffTimers(BuffData)
procedure HandleBuffTimers(var PowerUp: BuffData);
begin
	//check invincible expiry
	if (GetTimerSeconds(Powerup.Invincible.Clock) > BUFF_DURATION) then
	begin
		StopTimerPackage(Powerup.Invincible);
	end;
	//check speed expiry
	if (GetTimerSeconds(Powerup.Speed.Clock) > BUFF_DURATION) then
	begin
		StopTimerPackage(Powerup.Speed);
	end;
	//check damage expiry
	if (GetTimerSeconds(Powerup.Damage.Clock) > BUFF_DURATION) then
	begin
		StopTimerPackage(Powerup.Damage);
	end;
end;

//applies the specified powerup flag to the ship
//ActivatePowerupType(ShipData, ItemType)
procedure ActivatePowerupType(var Powerup: BuffData; const Buff: ItemType);
begin
	case Buff of
		Invincible:
		begin
			StartTimerPackage(Powerup.Invincible);
		end;
		Speed:
		begin
			StartTimerPackage(Powerup.Speed);
		end;
		Damage:
		begin
			StartTimerPackage(Powerup.Damage);
		end;
	end;
end;

//Manages the Buff activations
//HandleBuffState(BuffData, InventoryArray)
procedure HandleBuffState(var Powerup: BuffData; var Inventory: InventoryArray);
var
	i: Integer;
begin
	//checks for inventory item activation flags and marks the corresponding buff as active
	for i:=0 to High(Inventory) do
	begin
		if (Inventory[i].Activated) and not (Inventory[i].Drained) then
		begin
			ActivatePowerupType(Powerup, Inventory[i].ItemKind);
			Inventory[i].Drained := True;
		end;
	end;
end;

//check if the Ship has Moved out of the play area and Teleport it to the other side for seamless transitions
//HandleEdgeTransition(AmmoListArray);
procedure HandleEdgeTransition(var Ammo: AmmoData); overload;
begin
	//Play area top transition
	if (Ammo.Move.Pos.y < (-PLAY_HEIGHT/2)) then 
	begin
		TeleportShape(Ammo.Shape, Ammo.Move.Pos.x, PLAY_HEIGHT/2-20, Ammo.Move.Pos);
		Ammo.Move.Pos := PointAt(Ammo.Move.Pos.x, PLAY_HEIGHT/2-20);
	end
	//Play area right transition
	else if (Ammo.Move.Pos.x > PLAY_WIDTH/2) then
	begin
		TeleportShape(Ammo.Shape, -PLAY_WIDTH/2+20, Ammo.Move.Pos.y, Ammo.Move.Pos);
		Ammo.Move.Pos := PointAt(-PLAY_WIDTH/2+20, Ammo.Move.Pos.y);
	end
	//Play area bottom transition
	else if (Ammo.Move.Pos.y > PLAY_HEIGHT/2) then
	begin
		TeleportShape(Ammo.Shape, Ammo.Move.Pos.x, -PLAY_HEIGHT/2+20, Ammo.Move.Pos);
		Ammo.Move.Pos := PointAt(Ammo.Move.Pos.x, -PLAY_HEIGHT/2+20);
	end
	//Play area left transition
	else if (Ammo.Move.Pos.x < -PLAY_WIDTH/2) then
		begin
		TeleportShape(Ammo.Shape, PLAY_WIDTH/2-20, Ammo.Move.Pos.y, Ammo.Move.Pos);
		Ammo.Move.Pos := PointAt(PLAY_WIDTH/2-20, Ammo.Move.Pos.y);
	end;
end;

//check if the Ship has Moved out of the play area and Teleport it to the other side for seamless transitions
//CheckEdgeTransition(ShipData);
procedure HandleEdgeTransition(var Ship: ShipData); overload;
begin
	//Play area top transition
	if (Ship.Move.Pos.y < (-PLAY_HEIGHT/2 - Ship.Extents.w)) then
	begin
		TeleportShip(Ship, Ship.Move.Pos.x, PLAY_HEIGHT/2);
	end
	//Play area right transition
	else if (Ship.Move.Pos.x > (PLAY_WIDTH/2 + Ship.Extents.w)) then
	begin
		TeleportShip(Ship, -PLAY_WIDTH/2, Ship.Move.Pos.y);
	end
	//Play area bottom transition
	else if (Ship.Move.Pos.y > (PLAY_HEIGHT/2 + Ship.Extents.w)) then
	begin
		TeleportShip(Ship, Ship.Move.Pos.x, -PLAY_HEIGHT/2);
	end
	//Play area left transition
	else if (Ship.Move.Pos.x < (-PLAY_WIDTH/2 - Ship.Extents.w)) then
	begin
		TeleportShip(Ship, PLAY_WIDTH/2, Ship.Move.Pos.y);
	end;
end;

//Functionality for chase cam
//HandleCamera(Camera, Point2D);
procedure HandleCamera(const TargetPos: Point2D);
var
	xCamMargin, yCamMargin: Single;
begin
	//innitialise margin values for camera checking
	xCamMargin := PLAY_WIDTH/2-(WINDOW_WIDTH/2);
    yCamMargin := PLAY_HEIGHT/2-(WINDOW_HEIGHT/2);

    //refactor using case statements

	//check corners
	if (TargetPos.x < -xCamMargin) and (TargetPos.y < -yCamMargin) then //top-left
	begin
		MoveCameraTo(-PLAY_WIDTH/2, -PLAY_HEIGHT/2);
		Exit;
	end
	else if (TargetPos.x > xCamMargin) and (TargetPos.y < -yCamMargin) then //top-right
	begin
		MoveCameraTo(xCamMargin - (WINDOW_WIDTH/2), -PLAY_HEIGHT/2);
		Exit;
	end
	else if (TargetPos.x < -xCamMargin) and (TargetPos.y > yCamMargin) then //bottom-left
	begin
		MoveCameraTo(-PLAY_WIDTH/2, yCamMargin-(WINDOW_HEIGHT/2));
		Exit;
	end
	else if (TargetPos.x > xCamMargin) and (TargetPos.y > yCamMargin) then //bottom-right
	begin
		MoveCameraTo(xCamMargin-(WINDOW_WIDTH/2), yCamMargin-(WINDOW_HEIGHT/2));
		Exit;
	end;

	//check sides
	if (TargetPos.x < -xCamMargin) then //left
	begin
		MoveCameraTo(-PLAY_WIDTH/2, TargetPos.y-(WINDOW_HEIGHT/2));
		Exit;
	end
	else if (TargetPos.x > xCamMargin) then //right
	begin
		MoveCameraTo(xCamMargin-(WINDOW_WIDTH/2), TargetPos.y-(WINDOW_HEIGHT/2));
		Exit;
	end
	else if (TargetPos.y < -yCamMargin) then //top
	begin
		MoveCameraTo(TargetPos.x-(WINDOW_WIDTH/2), -PLAY_HEIGHT/2);
		Exit;
	end
	else if (TargetPos.y > yCamMargin) then //bottom
	begin
		MoveCameraTo(TargetPos.x-(WINDOW_WIDTH/2), yCamMargin-(WINDOW_HEIGHT/2));
		Exit;
	end;

	//Normal chase Cam
	MoveCameraTo(TargetPos.x-(WINDOW_WIDTH/2), TargetPos.y-(WINDOW_HEIGHT/2));
end;

//manage the state of the passed in ammo instance
//HandleAmmoState(AmmoData)
procedure HandleAmmoState(var Ammo: AmmoData);
begin
	HandleEdgeTransition(Ammo);
	//flag expired ammo for destruction
	if (GetTimerSeconds(Ammo.Expiry) > BALLISTIC_EXPIRY) then
	begin
		Ammo.IsAlive := False;
	end;
end;

//Parent handler for the passed in tool weapon
//HandleToolState(ToolData)
procedure HandleToolState(var Tool: ToolData; const Difficulty: DifficultyType; const Owner: OwnerType);
var
	i: Integer;
begin
	//Fire Tool
	if (Tool.CoolDown.Switch) then
	begin
		FireTool(Tool, Difficulty, Owner);		
		Tool.CoolDown.Switch := False;
	end;

	for i:=0 to High(Tool.SpawnedAmmo) do
	begin
		//Handle Tool's spawned ammo
		HandleAmmoState(Tool.SpawnedAmmo[i]);
	end;
end;

//Ship level controller for managing the state of the ship instance
//HandleShipState(ShipData, DifficultyType)
procedure HandleShipState(var Ship: ShipData; const Difficulty: DifficultyType);
var
	i: Integer;
begin
	AssertShipPointers(Ship);

	//tools
	for i:=0 to High(Ship.Tool) do
	begin
		HandleToolState(Ship.Tool[i], Difficulty, Ship.Owner);
	end;

	//edge transition and emitters
	if (Ship.IsAlive) then
	begin
		HandleEdgeTransition(Ship);
		HandleEmitters(Ship.Emitter, @Ship.Move.Heading);
	end;
end;

//returns the loot kind as a fresh new inventory item
//CreateInventoryItem(ItemType): ItemData
function CreateInventoryItem(LootKind: ItemType): ItemData;
begin
	Result.ItemKind := LootKind;
	Result.Activated := False;
	Result.Drained := False;
end;

//Manages loot pick up
//HandleLootPickup(ShipData, InventoryArray, LootDataArray)
procedure HandleLootPickup(const Ship: ShipData; var Inventory: InventoryArray; var LootArray: LootDataArray);
var
	i, f: Integer;
begin
	if (InvHasSpace(Inventory)) then
	begin
		f := GetEmptyInvIndex(Inventory);

		//iterate through all loot items for collision
		for i:=0 to High(LootArray) do
		begin
			if CollisionBetweenShapes(Ship.Shape, LootArray[i].Shape) and not (LootArray[i].PickedUp) then
			begin
				//put loot into inventory
				Inventory[f] := CreateInventoryItem(LootArray[i].LootKind);

				//flag loot for removal
				LootArray[i].PickedUp := True;
				Exit;
			end;
		end;
	end
end;

//manages number popup timers
//HandleNumberPopups(NumberPopupArray)
procedure HandleNumberPopups(var NumberPopups: NumberPopupArray);
var
	i: Integer;
begin
	for i:=0 to High(NumberPopups) do
	begin
		if (GetTimerSeconds(NumberPopups[i].Expiry) > NUMBER_POPUP_EXPIRY) then
		begin
			NumberPopups[i].IsAlive := False;
		end;
	end;
end;

//initialises and returns a movement model for debris
//GetDebrismovement(Point2D): MovementModel
function GetDebrisMovement(const Pos: Point2D): MovementModel;
begin	
	Result.Vel.x := (Random(MAX_EXPLOSION_VElOCITY*10)-(MAX_EXPLOSION_VElOCITY*10 div 2))/10;
	Result.Vel.y := (Random(MAX_EXPLOSION_VElOCITY*10)-(MAX_EXPLOSION_VElOCITY*10 div 2))/10;
	Result.MaxVel := MAX_EXPLOSION_VElOCITY;
	Result.TurnRate := ((Random(MAX_EXPLOSION_TURN_RATE)-MAX_EXPLOSION_TURN_RATE) * PI/180);
	Result.Pos := Pos;
	ClampVelocity(Result);
end;

//Creates and returns a debris entity from the passed in linesegment
//CreateDebris(LineSegment): Debris
function CreateDebris(const Line: LineSegment; const Color: LongWord): Debris;
begin
	SetLength(Result.Shape, 1);
	Result.Shape[0] := Line;

	//debris specific values
	Result.Move := GetDebrisMovement(LineMidPoint(Result.Shape[0]));
	Result.Color := Color;
	Result.IsAlive := True;
end;

//Handles killing a ship record
//KillShip(ShipData, DebrisListarray)
procedure KillShip(var Ship: ShipData; var DebrisList: DebrisListarray);
var
	NumToTransfer, NumInTarget, i: Integer;
	color: Longword;
begin
	//flag ship for removal
	Ship.IsAlive := False;

	//Create Debris from Ship's Shape
	NumToTransfer := Length(Ship.Shape);
	NumInTarget := Length(DebrisList);
	SetLength(DebrisList, numInTarget + numToTransfer);
	case Ship.Owner of
		HumanPlayer: Color := ColorAqua;
		Computer: Color := ColorRed;
	end;

	for i:=numInTarget to High(DebrisList) do
	begin
		DebrisList[i] := CreateDebris(Ship.Shape[i-numInTarget], color);
	end;	
	SetLength(Ship.Shape, 0);
end;

//Subcontroller for handling ship death events. Some stamp coupling, but I think the cohesiveness is worth it
//HandleShipDeath(ShipData, DebrisListArray, LootDataArray, LootDataArray, Integer)
procedure HandleShipDeath(var Ship: ShipData; var LevelData: Level; const NumberPopups: NumberPopupPtr);
begin
	if (Ship.Health <=0) then
	begin
		//Create debris from the ship's shape
		KillShip(Ship, LevelData.DebrisList);		

		//spawn loot and add score if ship is an NPC
		if (Ship.Owner = Computer) then
		begin
			SpawnRandomLoot(LevelData.LootList, Ship.Move.Pos); //spawn an item if dead ship is controlled by computer
			SpawnNumberPopup(GetShipMaxHealth(Ship.ShipKind), PointsPopup, Ship.Move.Pos, NumberPopups); //Points Popup
			LevelData.Score += GetShipMaxHealth(Ship.ShipKind); //add score
		end;
	end;
end;

//HAndles Debris state
//HandleDebris(DebrisListarray)
procedure HandleDebris(var DebrisList: DebrisListarray);
var
	i: Integer;
begin
	//Flag Debris for removal if it has basically stopped spinning
	for i:=0 to High(DebrisList) do
	begin
		if (abs(DebrisList[i].Move.TurnRate) < 0.005) then
		begin
			DebrisList[i].IsAlive := False;
		end;
	end;
end;

//Manages star twinkling states
//HandleStarTwinkling(StarArray, Timer)
procedure HandleStarTwinkling(var Stars: StarArray; const TwinkleCoolDown: Timer);
var
	i, r, TwinkleSize: Integer;
begin
	//guard conditional
	if (TWINKLE_RATE <= 0) then
	begin
		writeln('HandleStarTwinkling() - Invalid TWINKLE_RATE');
		ResetTimer(TwinkleCoolDown);
		Exit;
	end
	//Randomly select a portion of the total stars to twinkle
	else if (GetTimerSeconds(TwinkleCooldown) >= 1/TWINKLE_RATE) then
	begin
		for i:=0 to (High(Stars) div TWINKLE_PORTION) do
		begin
			TwinkleSize := Random(MAX_STAR_SIZE) + BASE_STAR_SIZE;
			r := Random(Length(Stars));
			Stars[r].Size := TwinkleSize;
		end;

		//decrease the size of twinkled stars over time
		for i:=0 to High(Stars) do
		begin
			if (Stars[i].Size > BASE_STAR_SIZE) then
			begin
				Stars[i].Size -=1;
			end;
		end;
		ResetTimer(TwinkleCoolDown);
	end;
end;

//explodes the wormhole into fancy debris
//ExplodeWormhole(Boolean, DebrisListArray
procedure ExplodeWormhole(const Wormhole: WormholeData; var DebrisList: DebrisListArray);
var
	f: Integer;
	Shape: LinesArray;
begin
	if not (Wormhole.IsAlive) then
	begin
		AddLine(Shape, 0, 0, LINE_LENGTH, LINE_LENGTH, PointAt(0,0));
		
		SetLength(DebrisList, Length(DebrisList)+1);
		f := High(DebrisList);

		DebrisList[f] := CreateDebris(Shape[0], ColorYellow);
	end;
end;

//Manages wormhole isalive state
//HandleWormholeState(WormholeData)
procedure HandleWormholeState(var Wormhole: WormholeData; const Difficulty: DifficultyType);
var
	rate, constant: single;
begin
	if (CircleRadius(Wormhole.Shape) < LINE_LENGTH*2) then
	begin
		Wormhole.IsAlive := False;
	end
	else
	begin
		rate := 1 + (WORMHOLE_GROWTH_RATE/100)/FPS * GetDifficultyModifier(Difficulty);
		constant := WORMHOLE_CONSTANT_GROWTH/FPS * GetDifficultyModifier(Difficulty);
		Wormhole.Health := (Wormhole.Health * rate) + constant;
		Wormhole.Shape.Radius := (Wormhole.Health/WORMHOLE_BASE_HEALTH) * WORMHOLE_BASE_RADIUS;
	end;
end;

//Ship level controller for array management
//CleanShip(ShipData)
procedure CleanShip(var Ship: ShipData);
var
	i: Integer;
begin
	for i:=0 to High(Ship.Tool) do
	begin		
		RemoveDead(Ship.Tool[i].SpawnedAmmo);
	end;

	for i:=0 to High(Ship.Emitter) do
	begin
		RemoveDead(Ship.Emitter[i].Particles);
	end;
end;

//Array of Ships level controller for array management
//CleanShipArray(ShipDataArray)
procedure CleanShipArray(var ShipArray: ShipDataArray);
var
	i: Integer;
begin
	//Remove expired ships from the ship array
	RemoveDead(ShipArray);

	//Clean remaning ships
	for i:=0 to High(ShipArray) do
	begin
		CleanShip(ShipArray[i]);
	end;
end;

//Subcontroller for array cleanup of expired and dead instances
//HandlearrayCleaning(Player, NPCTierArray, Level)
procedure HandlearrayCleaning(var PlayerData: Player; var NPCData: NPCTierArray; var LevelData: Level);
var
	i: Integer;
begin
	//Player arrays
	CleanShip(PlayerData.Ship);
	RemoveDead(PlayerData.NumberPopups); //Expired number popups
	RemoveDead(PlayerData.Inventory); //Used Items

	//NPC arrays
	for i:=0 to High(NPCData) do
	begin
		CleanShipArray(NPCData[i].Ships)
	end;

	//Level arrays
	RemoveDead(LevelData.DebrisList); //Level Expired Debris
	RemoveDead(LevelData.LootList); //Level Picked up Loot
end;

//Sub controller for handling the players state
//HandlePlayerState(Player, LootDataArray, DifficultyType)
procedure HandlePlayerState(var PlayerData: Player; var LevelData: Level; const Difficulty: DifficultyType);
begin
	HandleShipState(PlayerData.Ship, Difficulty);
	HandleShipDeath(PlayerData.Ship, LevelData, @PlayerData.NumberPopups);

	HandleNumberPopups(PlayerData.NumberPopups);

	if (PlayerData.Ship.IsAlive) then
	begin
		HandleLootPickup(PlayerData.Ship, PlayerData.Inventory, LevelData.LootList);
		UpdateInventoryBmp(PlayerData.Inventory, PlayerData.UI);
		HandleBuffState(PlayerData.Ship.Powerup, PlayerData.Inventory);
		HandleBuffTimers(PlayerData.Ship.Powerup);
	end;
end;

//subcontroller for the state of NPC ships
//HandleNPCShip(array of ShipData, Point2D, DifficultyType)
procedure HandleNPCShip(var Ship: ShipData; const TargetPos: Point2D; const Difficulty: DifficultyType);
begin
	HandleNPCBehaviour(Ship, TargetPos, Difficulty);
	HandleShipState(Ship, Difficulty);
end;

//Sub controller for handling active npc behaviour
//HandleNPCShips(NPCTier, Level, Point2D, DifficultyType)
procedure HandleNPCShips(var ShipArray: ShipDataArray; const PlayerPos: Point2D; const Difficulty: DifficultyType);
var
	i: Integer;
begin
	for i:=0 to High(ShipArray) do
	begin
		HandleNPCShip(ShipArray[i], PlayerPos, Difficulty);
	end;
end;

//NPC Tier level manage for npc ship death
//HandleShipArrayDeath(NPCTier, Level, NumberPopupPtr)
procedure HandleShipArrayDeath(var ShipArray: ShipDataArray; var LevelData: Level; const NumberPopups: NumberPopupPtr);
var
	i: Integer;
begin
	for i:=0 to High(ShipArray) do
	begin
		HandleShipDeath(ShipArray[i], LevelData, NumberPopups);
	end;
end;

//manages the starry level background
//HandleBackground(BackgroundData)
procedure HandleBackground(var Background: BackgroundData);
begin
	HandleStarTwinkling(Background.Stars, Background.TwinkleCooldown);
end;

//Sub controller for handling the level state
//HandleLevelState(Level, Point2D)
procedure HandleLevelState(var LevelData: Level; const PlayerPos: Point2D; const Difficulty: DifficultyType);
begin
	HandleCamera(PlayerPos);
	HandleDebris(LevelData.DebrisList);
	HandleBackground(LevelData.Background);

	//wormhole
	if (LevelData.Wormhole.IsAlive) then
	begin
		HandleWormholeState(LevelData.Wormhole, Difficulty);
	end
	else
	begin
		ExplodeWormhole(LevelData.Wormhole, LevelData.DebrisList);
		StopTimer(LevelData.SpawnTimer);
	end;
end;

//Main controller for managing the game state and game rules
//HandleGameState(Player, Level, NPCTierArray, DifficultyType)
procedure HandleGameState(var PlayerData: Player; var LevelData: Level; var NPCData: NPCTierArray; const Difficulty: DifficultyType);
var
	i: Integer;
	targetPos: Point2D;
begin
	//Player
	HandlePlayerState(PlayerData, LevelData, Difficulty);

	//NPC
	targetPos := PlayerData.Ship.Move.Pos;
	for i:=0 to High(NPCData) do
	begin
		HandleNPCShips(NPCData[i].Ships, targetPos, Difficulty);
		HandleShipArrayDeath(NPCData[i].Ships, LevelData, @PlayerData.NumberPopups);
	end;

	//Level
	HandleLevelState(LevelData, PlayerData.Ship.Move.Pos, Difficulty);

	//Collisions
	HandleCollisions(PlayerData, LevelData.Wormhole, NPCData);

	//Array cleaning
	HandleArrayCleaning(PlayerData, NPCData, LevelData);
end;

//////////////////
// MainGame() Handle Player Input
//////////////////

//returns whether player wants to accelerate forward
//RotateRightInput(): Boolean
function AccelerateForwardInput(): Boolean;
begin
	Result := False;
	If KeyDown(WKey) or KeyDown(UpKey) then
	begin
		Result := True;
	end;
end;

//returns whether pplayer wants to accelerate backwards
//RotateLeftInput(): Boolean
function AccelerateBackwardInput(): Boolean;
begin
	Result := False;
	If KeyDown(SKey) or KeyDown(Downkey) then
	begin
		Result := True;
	end;
end;

//returns whether player wants to rotate right
//RotateRightInput(): Boolean
function RotateRightInput(): Boolean;
begin
	Result := False;
	If KeyDown(DKey) or KeyDown(RightKey) then
	begin
		Result := True;
	end;
end;

//returns whether pplayer wants to rotate left
//RotateLeftInput(): Boolean
function RotateLeftInput(): Boolean;
begin
	Result := False;
	If KeyDown(AKey) or KeyDown(LeftKey) then
	begin
		Result := True;
	end;
end;

//Handles Player movement inputs
//HandleMovementInput(MovementModel, array of Emitter)
procedure HandleMovementInput(var Move: MovementModel; var Emitter: array of EmitterData);
var	
	StrafeHeading: Vector;
begin
	//forward and backwards
	if AccelerateForwardInput() and not AccelerateBackwardInput() then
	begin
		Move.vel.x += Move.Heading.x * Move.Accel;
		Move.vel.y += Move.Heading.y * Move.Accel;
		ActivateEmittersNamed(Emitter, 'Thruster');
	end;
	if AccelerateBackwardInput() and not AccelerateForwardInput() then
	begin
		Move.vel.x += -Move.Heading.x * Move.Accel * Move.ReverseMod;
		Move.vel.y += -Move.Heading.y * Move.Accel * Move.ReverseMod;
		ActivateEmittersNamed(Emitter, 'Thruster');
	end;

	//strafe left and right
	//get strafe heading variable to help find actual strafe vectors
	StrafeHeading := VectorTo(0,0);
	if KeyDown(QKey) then
	begin
		StrafeHeading := RotateVector(Move.Heading, -PI/2);
		Move.vel.x += StrafeHeading.x * Move.Accel * Move.StrafeMod;
		Move.vel.y += StrafeHeading.y * Move.Accel * Move.StrafeMod;
		ActivateEmittersNamed(Emitter, 'Thruster');
	end;
	if KeyDown(EKey) then
	begin
		StrafeHeading := RotateVector(Move.Heading, PI/2);
		Move.vel.x += StrafeHeading.x * Move.Accel * Move.StrafeMod;
		Move.vel.y += StrafeHeading.y * Move.Accel * Move.StrafeMod;
		ActivateEmittersNamed(Emitter, 'Thruster');
	end;

	//Rotate left and right
	if RotateLeftInput() and not RotateRightInput() then
	begin
		Move.TargetHeading := RotateVector(Move.Heading, -1 * Move.TurnRate);
	end;
	if RotateRightInput() and not RotateLeftInput() then
	begin
		Move.TargetHeading := RotateVector(Move.Heading, Move.TurnRate);
	end;
end;

//handles player action inputs like item activation and tool weapon firing
//HandleActionInput(array of ToolData, InventoryArray, BuffData)
procedure HandleActionInput(var Tool: array of ToolData; var Inventory: InventoryArray; const Powerup: BuffData);
var
	i: Integer;
begin
	//Fire Tool Weapons
	if KeyDown(SpaceKey) then
	begin
		for i:=0 to High(Tool) do
		begin
			if (Tool[i].AmmoKind = Ballistic) then
			begin
				Tool[i].CoolDown.Switch := True;
			end;
		end;
	end;

	//Activate BuffData
	if KeyTyped(XKey) then
	begin
		for i:=0 to High(Inventory) do
		begin
			if CanActivateItem(Inventory[i], Powerup) then
			begin
				Inventory[i].Activated := True;
				Break;
			end;
		end;
	end;
end;

//Handles player input to the game state
//HandleGameInput(ShipData, InventoryArray);
procedure HandleGameInput(var Ship: ShipData; var Inventory: InventoryArray);
begin
	ProcessEvents();

	if (Ship.IsAlive) then
	begin
		HandleMovementInput(Ship.Move, Ship.Emitter);
		HandleActionInput(Ship.Tool, Inventory, Ship.Powerup);
	end;
end;

//////////////////
// MainGame() Setup Game Procedures
//////////////////

//Returns the tier color from the passed in NPCTier Index
//GetNPCTierColor(Integer): LongWord
function GetNPCTierColor(Index: Integer): LongWord;
begin
	case Index of
		0: Result := ColorBurlyWood;
		1: Result := ColorSilver;
		2: Result := ColorGold;
		else Result := ColorBurlyWood;
	end;
end;

//setup the ships within the NPC tiers
//SetupNPCTierShips(NPCTier, ShipType, LongWord, DifficultyType)
procedure SetupNPCTierShips(var ShipArray: ShipDataArray; const ShipKind: ShipType; const BaseColor: LongWord; const Difficulty: DifficultyType);
var
	i: Integer;
begin
	for i:=0 to High(ShipArray) do
	begin
		ShipArray[i] := CreateShip(ShipKind, BaseColor, PointAt(0, 0), Computer, Difficulty);
		AssertShipPointers(ShipArray[i]);
	end;	
end;

//Setup the NPC Tiers
//SetupNPCData(NPCTierArray, DifficultyType)
procedure SetupNPCData(var NPCData: NPCTierArray; const Difficulty: DifficultyType);
var
	i: Integer;
	BaseColor: LongWord;
begin
	//create up to 3 tiers of enemy types based on difficulty setting
	case Difficulty of
		Novice: SetLength(NPCData, 1);
		Intermediate: SetLength(NPCData, 2);
		Expert: SetLength(NPCData, 3);
		else WriteLn('SetupNPC() - invalid difficulty');
	end;

	for i:=0 to High(NPCData) do
	begin
		//split NPC_COUNT between tiers modified by the difficulty modifier
		SetLength(NPCData[i].Ships, Round(NPC_COUNT/Length(NPCData) * GetDifficultyModifier(Difficulty)));

		BaseColor := GetNPCTierColor(i);
		SetupNPCTierShips(NPCData[i].Ships, ShipType(i), BaseColor, Difficulty);
	end;
end;

//Setup the player Ship
//SetupPlayerUnit(ShipData, ShipKind);
procedure SetupPlayerShip(var Ship: ShipData; const ShipKind: ShipType);
begin
	Ship := CreateShip(ShipKind, ColorAqua, GetStartingPosition(), HumanPlayer);
	AssertShipPointers(Ship);

	//player specific values placeholder
	Ship.Move.Accel *= PLAYER_SPEED_BUFF;
	Ship.Move.MaxVel *= PLAYER_SPEED_BUFF;
end;

//returns an empty inventory record
//GetEmptyInventory(): InventoryArray
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

//Load Bitmaps into containers for drawing the player UI
//GetUIBitmaps(): UIData;
function GetUIBitmaps(): UIData;
begin
	Result.SpeedBuffIcon := BitmapNamed('SpeedBuff');
	Result.InvincibleBuffIcon := BitmapNamed('InvincibleBuff');
	Result.DamageBuffIcon := BitmapNamed('DamageBuff');
	Result.HealthContainer := BitmapNamed('HealthContainer');
	Result.WHMeterContainer := BitmapNamed('VerticalContainer');
end;

//Setup player values
//SetupPlayer(player, ShipType)
procedure SetupPlayer(var PlayerData: Player; const ShipKind: ShipType);
begin
	//setup the player ship
	SetupPlayerShip(PlayerData.Ship, ShipKind);
	Playerdata.UI := GetUIBitmaps();
	PlayerData.Inventory := GetEmptyInventory();
end;

//initialises and returns a wormhole record
//GetWormhole(Single, DifficultyType, OwnerType): WormholeData
function GetWormhole(Radius: Single; const Difficulty: DifficultyType; Owner: OwnerType): WormholeData;
begin
	Result.Owner := Owner;
	Result.IsAlive := True;
	Result.Shape := CircleAt(0,0, Radius);
	Result.Color[0] := ColorBlack;
	Result.Color[1] := ColorBlueViolet;
	Result.Health := WORMHOLE_BASE_HEALTH * GetDifficultyModifier(Difficulty);
end;

//returns array of star entities
//option to offset the star positions e.g. 0.5 offsets 50% positive coord direction
//CreateStars(Single, Single, Single): StarArray
function CreateStars(w, h: Single; OffsetOpt: Single = 0): StarArray;
var
	count, i: Integer;
begin
	count := Round(STAR_COUNT * ((w*h)/(PLAY_WIDTH*PLAY_HEIGHT)));
	SetLength(Result, count);
	for i:=0 to High(Result) do
	begin
		Result[i].Pos := GetRandomPointWithin(w, h, OffsetOpt);
		Result[i].Size := BASE_STAR_SIZE;
	end;
end;

//builds and returns an array of background stars
//CreateBackground(): BackgroundData
function CreateBackground(): BackgroundData;
begin	
	Result.Stars := CreateStars(PLAY_WIDTH, PLAY_HEIGHT);	
	Result.TwinkleCoolDown := CreateTimer();
	StartTimer(Result.TwinkleCooldown);
end;

//Setup level data
//SetupLevel(Level, DifficultyType);
procedure SetupLevel(var LevelData: Level; const Difficulty: DifficultyType);
begin
	//Setup the starry background
	LevelData.Background := CreateBackground();

	//Setup wormhole
	LevelData.Wormhole := GetWormhole(WORMHOLE_BASE_RADIUS, Difficulty, Computer);

	//Setup Score
	LevelData.Score := 0;

	//spawn timer
	LevelData.SpawnTimer := CreateTimer();
	StartTimer(LevelData.SpawnTimer);
end;

//controller for setting up the initial game state
//SetupGame(Game, DifficultyType, ShipType)
procedure SetupGame(var GameModule: Game; const Difficulty: DifficultyType; const ShipKind: ShipType);
begin
	Randomize;
	LoadDefaultColors();

	SetupLevel(GameModule.LevelData, Difficulty);
	SetupPlayer(GameModule.PlayerData, ShipKind);
	SetupNPCData(GameModule.NPCData, Difficulty);
end;

//////////////////
// MainGame() NPC Spawning procedures
//////////////////

//returns whether we can spawn a new npc ship
//CanSpawnNPC(DifficultyType, Timer): Boolean
function CanSpawnNPC(const Difficulty: DifficultyType; const SpawnTimer: Timer): Boolean;
var
	SpawnTime: Single;
begin
	Result := False;
	SpawnTime := SPAWN_TIME / GetDifficultyModifier(Difficulty);
	if (GetTimerSeconds(SpawnTimer) > SpawnTime) then
	begin
		Result := True;
	end;
end;

//Spawns a ship entity into the npc tier
//SpawnNPCTierShip(NPCTier, LongWord)
procedure SpawnNPCTierShip(var ShipArray: ShipDataArray; const ShipKind: ShipType; const BaseColor: LongWord; const Difficulty: DifficultyType);
var
	f: Integer;
begin
	//create new ship entry
	SetLength(ShipArray, Length(ShipArray) + 1);
	f := High(ShipArray);

	//build the ship
	ShipArray[f] := CreateShip(ShipKind, BaseColor, PointAt(0, 0), Computer, Difficulty);
	AssertShipPointers(ShipArray[f]);
end;

//Manages the spawning of npc entities
//HandleNPCSpawning(NPCTierArray, DifficultyType, Timer)
procedure HandleNPCSpawning(var NPCData: NPCTierArray; const Difficulty: DifficultyType; const SpawnTimer: Timer);
var
	i: Integer;
	BaseColor:  LongWord;
begin
	if (TimerTicks(SpawnTimer) <> 0) then
	begin
		if CanSpawnNPC(Difficulty, SpawnTimer) then
		begin
			for i:=0 to High(NPCData) do
			begin
				BaseColor := GetNPCTierColor(i);
				SpawnNPCTierShip(NPCData[i].Ships, NPCData[i].ShipKind, BaseColor, Difficulty);
				ResetTimer(SpawnTimer);
			end;
		end;
	end;
end;

//////////////////
// MainGame()
//////////////////

function MainGame(const Difficulty: DifficultyType; const ShipKind: ShipType): ReceiveData;
var
	GameModule: Game;
	GameTime, EndDelay: Timer;
	EndMainGame: Boolean = False;
	Win: Boolean = False;
begin
	LoadResourceBundleNamed('game_module', 'game_bundle.txt', true);
	PlayMusic('GameMusic');

	try
		SetupGame(GameModule, Difficulty, ShipKind);
		GameTime := CreateTimer();
		EndDelay := CreateTimer();
		StartTimer(GameTime);
	except
		WriteLn('Error setting up game');
		Exit;
	end;

	repeat
		ClearScreen(ColorBlack);

		HandleGameInput(GameModule.PlayerData.Ship, GameModule.PlayerData.Inventory);
		HandleGameState(GameModule.PlayerData, GameModule.LevelData, GameModule.NPCData, Difficulty);
		HandleNPCSpawning(GameModule.NPCData, Difficulty, GameModule.LevelData.SpawnTimer);
		UpdateGamePositions(GameModule, Difficulty);
		DrawGame(GameModule.PlayerData, GameModule.NPCData, GameModule.LevelData, Difficulty);

		//Handle end game condition
		Win := IsPlayerWin(GameModule.LevelData.Wormhole, GameModule.PlayerData.Ship);
		EndMainGame := BattleEndDelay(EndDelay, 4, GameModule.LevelData.Wormhole.IsAlive, GameModule.PlayerData.Ship.IsAlive);

		RefreshScreen(FPS);
	until WindowCloseRequested() or EndMainGame;

	Result.Score := GameModule.LevelData.Score;
	Result.Win := Win;
	Result.Time := Round(GetTimerSeconds(GameTime));
	StopTimer(GameTime);

	ReleaseAllTimers();
	StopMusic();
	ReleaseResourceBundle('game_module');
end;

//////////////////
// MainGame/Menus Interface
//////////////////

//Interface module between the menus and the maingame
//Menu module sends data to the Maingame module and receives data back when the game finishes
//MainGameInterface(SendData): ReceiveData
function MainGameInterface(const Send: SendData): ReceiveData;
begin
	//Start MainGame
	Result := MainGame(Send.Difficulty, Send.ShipClass);
end;

//////////////////
// Draw Menus
//////////////////

//returns a padding ammount for centering things within containers nicely
//GetPadding(Single, Single): Single
function GetPadding(TextSize, BoxSize: Single): Single;
begin
	Result := (BoxSize - TextSize) / 2;
end;

//Draws a button with the passed in attributes
//DrawButton(String, Point2D, Size, LongWord, LongWord)
procedure DrawButton(const Text: String; const Pos: Point2D; const Extents: Size; const FillColor, TextColor: LongWord);
var
	TextWidth: Single;
begin
	FillRectangle(FillColor, Pos.x, Pos.y, Extents.w, Extents.h);
	DrawRectangle(TextColor, Pos.x, Pos.y, Extents.w, Extents.h);
	TextWidth := Length(Text) * 10;
	DrawText(Text, TextColor, 'ButtonText', Pos.x + GetPadding(TextWidth, Extents.w), Pos.y + Extents.h/6);
end;

//Handles button drawing from an array of buttons
//DrawMenuButtons(Buttons)
procedure DrawMenuButtons(const Buttons: ButtonArray);
var
	i: Integer;
	Pos: Point2D;
	Extents: Size;
begin
	for i:=0 to High(Buttons) do
	begin
		Pos := PercentToPosPoint(Buttons[i].Pos);
		Extents := PercentToSizeExtent(Buttons[i].Extents);

		//draw volume button
		if (Buttons[i].Payload.Kind = Volume) then
		begin
			Extents.w *= MusicVolume();
			DrawButton(Buttons[i].ButtonName, Pos, Extents, ColorRed, Buttons[i].Color[1]);
		end
		//draw text input button
		else if (Buttons[i].Action = TextInput) then
		begin
			DrawButton(Buttons[i].ButtonName, Pos, Extents, ColorWhite, Buttons[i].Color[1]);
			if not ReadingText() then
			begin
				DrawText(Buttons[i].Payload.Str, ColorBlack, 'MenuText', Round(Pos.x) + 10, Round(Pos.y + GetPadding(28, Extents.h)));
			end;
		end
		//draw all other buttons
		else
		begin
			case Buttons[i].Highlighted of
				True: DrawButton(Buttons[i].ButtonName, Pos, Extents, ColorYellow, ColorBlack);
				False: DrawButton(Buttons[i].ButtonName, Pos, Extents, Buttons[i].Color[0], Buttons[i].Color[1]);
			end;
		end;
	end;
end;

//Draws all Menu textboxes to screen
//DrawMenuText(TextBoxArray)
procedure DrawMenuText(const TextBoxes: TextBoxArray);
var
	i: Integer;
	Pos: Point2D;
begin
	for i:=0 to High(TextBoxes) do
	begin
		Pos := PercentToPosPoint(TextBoxes[i].Pos);
		DrawText(TextBoxes[i].Text, TextBoxes[i].Color, 'MenuText', Pos.x, Pos.y);
	end;

	//Draw Name
	DrawText('2018 Jeremy Vun 2726092', ColorWhite, 'DamagePopup', 4, WINDOW_HEIGHT - 18);
end;

//draws the menu title to screen
//DrawMenuTitle(String)
procedure DrawMenuTitle(const MenuName: String);
var
	Pos: Point2D;
begin
	Pos := PercentToPosPoint(PointAt(0,0.13));
	Pos.x := GetPadding(Length(MenuName) * 21, WINDOW_WIDTH);
	DrawText(MenuName, ColorWhite, 'MenuTitle', Pos.x, Pos.y);
end;

//Controller for drawing the currently selected menu to the screen
//DrawMenu(Menu, BackgroundData)
procedure DrawMenu(const CurrMenu: Menu; const Background: BackgroundData);
begin
	if not WindowCloseRequested() then
	begin
		DrawStars(Background.Stars);
		DrawMenuTitle(CurrMenu.MenuName);
		DrawMenuText(CurrMenu.TextBoxes);
		DrawMenuButtons(CurrMenu.Buttons);
	end;
end;

//////////////////
// Handle Menu State
//////////////////

//returns the payload string of the button with the specified button name
//GetButtonNamedPayload(ButtonArray, String)
function GetButtonNamedPayload(const Buttons: ButtonArray; const ButtonName: String): String;
var
	i: Integer;
begin
	for i:=0 to High(Buttons) do
	begin
		if Buttons[i].ButtonName = ButtonName then
		begin
			Result := Buttons[i].Payload.Str;
			Exit;
		end;
	end;
end;

//adds a textbox to the end of the array
//AddTextBox(TextBoxArray, String, Point2D, LongWord): TextBoxArray
function AddTextBox(TextBoxes: TextBoxArray; const Text: String; const Pos: Point2D; const Color: LongWord): TextBoxArray;
var
	f: Integer;
begin
	SetLength(TextBoxes, Length(TextBoxes)+1);
	f := High(TextBoxes);

	TextBoxes[f].Text := Text;
	TextBoxes[f].Pos := Pos;
	TextBoxes[f].Color := Color;

	Result := TextBoxes;
end;

//adds time text to the end of the array and returns it
//TimeText(TextBoxArray, Integer): TextBoxArray
function TimeText(TextBoxes: TextBoxArray; const Time: Integer): TextBoxArray;
var
	line: String;
begin
	line := Concat('You Survived for: ', IntToStr(Time), ' seconds');
	Result := AddTextBox(TextBoxes, line, PointAt(0.28, 0.6), ColorWhite);
end;

//adds score text to the end of the array and returns it
//ScoreText(TextBoxArray, Integer): TextBoxArray
function ScoreText(TextBoxes: TextBoxArray; const Score: Integer): TextBoxArray;
var
	line: String;
begin
	line := Concat('Your Score: ', IntToStr(Score));
	Result := AddTextBox(TextBoxes, line, PointAt(0.38, 0.5), ColorWhite);
end;

//adds and returns text indicating whether score is a highscore
//HighScoreText(TextBoxArray, Boolean): TextBoxArray
function HighScoreText(TextBoxes: TextBoxArray; const HighScore: Boolean): TextBoxArray;
var
	line: String;
begin
	if HighScore then
	begin
		line := 'NEW HIGHSCORE!';
		Result := AddTextBox(TextBoxes, line, PointAt(0.36, 0.4), ColorYellow);
	end
	else Result := TextBoxes;
end;

//adds and returns win/lose text
//WinLoseText(TextBoxArray, Boolean): TextBoxArray
function WinLoseText(TextBoxes: TextBoxArray; const Win: Boolean): TextBoxArray;
var
	line: String;
	color: LongWord;
begin
	if Win then
	begin
		line := 'You Win';
		color := ColorAqua;
	end
	else
	begin
		line := 'You Lose';
		color := ColorRed;
	end;

	Result := AddTextBox(TextBoxes, line, PointAt(0.43,0.35), color);
end;

//populates and returns scorescreen textboxes
//CreateScoreScreen(ReceiveData, Boolean): TextBoxArray
function CreateScoreScreen(const Receive: ReceiveData; const HighScore: Boolean): TextBoxArray;
var
	TextBoxes: TextBoxArray;
begin
	//build up the textbox array sequentially
	SetLength(TextBoxes, 0);
	TextBoxes := WinLoseText(TextBoxes, Receive.Win);
	TextBoxes := HighScoreText(TextBoxes, HighScore);
	TextBoxes := ScoreText(TextBoxes, Receive.Score);
	TextBoxes := TimeText(TextBoxes, Receive.Time);

	Result := TextBoxes;
end;

//validation check whether all the save actiontype buttons have been selected (difficulty and ship class)
//AllOptionsSelected(ButtonArray): Boolean;
function AllOptionsSelected(const Buttons: ButtonArray): Boolean;
var
	i: Integer;
	DiffSelected, ShipSelected: Boolean;
begin
	Result := False;
	DiffSelected := False;
	ShipSelected := False;

	for i:=0 to High(Buttons) do
	begin
		if (Buttons[i].Payload.Kind = Difficulty) and (Buttons[i].Highlighted) then
		begin
			DiffSelected := True;
		end;
		if (Buttons[i].Payload.Kind = ShipClass) and (Buttons[i].Highlighted) then
		begin
			ShipSelected := True;
		end;
		if (DiffSelected) and (ShipSelected) then
		begin
			Result := True;
		end;
	end;
end;

//returns whether the player has requested to start the Root Game
//CheckGameStart(MenuType, ButtonArray): Boolean
function CheckGameStart(var Buttons: ButtonArray): Boolean;
var
	i: Integer;
begin
	Result := False;
	for i:=0 to High(Buttons) do
	begin
		if (Buttons[i].Action = Play) and (Buttons[i].Clicked = True) then
		begin
			Buttons[i].Clicked := False;
			if AllOptionsSelected(Buttons) then
			begin
				Result := True;
			end
			else PlaySoundEffect('Error');
			Exit;
		end;
	end;
end;

//Changes the current menu pointer to the menu of the passed in type
//ChangeCurrentMenu(MenuType, MenuPtr, MenuArray)
procedure ChangeCurrentMenu(MenuKind: MenuType; var CurrentMenu: MenuPtr; const Menus: MenuArray);
var
	i: Integer;
begin
	for i:=0 to High(Menus) do
	begin
		if (Menus[i].MenuKind = MenuKind) then
		begin
			CurrentMenu := @Menus[i];
			Exit;
		end
	end;	
	WriteLn('HandleMenuChange() - Invalid Button Payload specified');
end;

//Iterates through the button array and responds to user clicking on a navto button type
//HandleMenuChange(ButtonArray, MenuPtr, MenuArray)
procedure HandleMenuChange(var Buttons: ButtonArray; var CurrentMenu: MenuPtr; const Menus: MenuArray);
var
	MenuKind: MenuType;
	i: Integer;
begin
	for i:=0 to High(Buttons) do
	begin
		if (Buttons[i].Clicked) and (Buttons[i].Action = NavTo) then
		begin
			Buttons[i].Clicked := False;
			ReadStr(Buttons[i].Payload.Str, MenuKind);
			ChangeCurrentMenu(MenuKind, CurrentMenu, Menus);
		end;
	end;
end;

//clears the Highlights of all buttons of the specified Payload type group
//ClearBtnGroupHighlights(ButtonArray, PpayloadType): ButtonArray
function ClearBtnGroupHighlights(Buttons: ButtonArray; Kind: PayloadType): ButtonArray;
var
	i: Integer;
begin
	for i:=0 to High(Buttons) do
	begin
		if (Buttons[i].Payload.Kind = Kind) then
			Buttons[i].Highlighted := False;
	end;

	Result := Buttons;
end;

//Handles select menu behaviour for saving and sending player selected data to the game interface
//HandleSelectMenuButtons(ButtonArray, SendData)
procedure HandleSelecMenuButtons(var Buttons: ButtonArray; var Send: SendData);
var
	Saved: Boolean = False;
	i: Integer;
begin
	for i:=0 to High(Buttons) do
	begin
		if (Buttons[i].Clicked) and (Buttons[i].Action = Save) then
		begin
			Buttons[i].Clicked := False;

			//save the button payload for sending to maingame()
			try
				case Buttons[i].Payload.Kind of
					Difficulty: ReadStr(Buttons[i].Payload.Str, Send.Difficulty);
					ShipClass: ReadStr(Buttons[i].Payload.Str, Send.ShipClass);
				end;
				Saved := True;
			except
				WriteLn('HandleSelectMenuButtons() Error detected');
			end;

			//Reset Highlighting to draw user feedback about what has been selected/saved
			if (Saved) then
			begin				
				Buttons := ClearBtnGroupHighlights(Buttons, Buttons[i].Payload.Kind);
				Buttons[i].Highlighted := True;
			end;
		end;
	end;
end;

//Parent controller for handling the state of the menu module
//HandleMenuState(MenuDAta, SendData)
procedure HandleMenuState(var MenuData: MenuData; var GameInterfaceSend: SendData);
begin
	HandleBackground(MenuData.Background);
	HandleMenuChange(MenuData.CurrentMenu^.Buttons, MenuData.CurrentMenu, MenuData.Menus);	
	HandleSelecMenuButtons(MenuData.CurrentMenu^.Buttons, GameInterfaceSend);
end;

//////////////////
// Write Data to Menu textboxes
//////////////////

//inserts a string into the textbox array at the specified index
//InsertTextBoxAt(TextBoxArray, Integer, String);
procedure InsertTextBoxAt(var TextBoxes: TextBoxArray; const Index: Integer; const Str: String);
var
	i: Integer;
begin
	i := High(TextBoxes);
	while i > Index do
	begin
	 	TextBoxes[i].Text := TextBoxes[i-1].Text;
	 	i -= 1;
	end;

	TextBoxes[Index].Text := Str;
end;

//inserts score and name into the highscore textbox array if score is high enough and returns the modified textbox array
//InsertScore(TextBox, Score, PlayerName)
function InsertScore(TextBoxes: TextBoxArray; const Score: Integer; const Playername: String): TextBoxArray;
var
	i: Integer;
	num: Integer;
begin
	for i:=0 to High(TextBoxes) do
	begin
		if TryStrToInt(TextBoxes[i].Text, num) and (score > num) then
		begin
			InsertTextBoxAt(TextBoxes, i-1, Playername);
			InsertTextBoxAt(TextBoxes, i, IntToStr(score));
			Break;
		end;
	end;

	result := TextBoxes;
end;

///Controller for updating highscore textboxes
//UpdateHighScores(MenuPtr, Integer, String)
procedure UpdateHighScores(const HighScoreMenu: MenuPtr; const Score: Integer; const Playername: String);
begin
	HighScoreMenu^.TextBoxes := InsertScore(HighScoreMenu^.TextBoxes, Score, Playername);
end;

//////////////////
// Handle Menu Input
//////////////////

//Writes text input from player into the button payload
//WriteTextToButtonPayload(ButtonArray)
procedure WriteTextToButtonPayload(var Buttons: ButtonArray);
var
	i: Integer;
begin
	for i:=0 to High(Buttons) do
	begin
		if (Buttons[i].Action = TextInput) then
		begin
			Buttons[i].Payload.Str := EndReadingText();
		end;
	end;
end;

//iterates through all buttons and activates any button that contains the passed in x,y pos
//ButtonActionOnClick(ButtonArray, Point2D)
procedure ButtonActionOnClick(var Buttons: ButtonArray; const ClickPos: Point2D);
var
	i: Integer;
	vol: Single;
	Extents: Size;
	Pos: Point2D;
	ReadFont: Font;
begin
	for i:=0 to High(Buttons) do
	begin
		if PointWithinBox(ClickPos, PercentToPosPoint(Buttons[i].Pos), PercentToSizeExtent(Buttons[i].Extents)) then
		begin
			Extents := PercentToSizeExtent(Buttons[i].Extents);
			Pos := PercentToPosPoint(Buttons[i].Pos);

			//read text from player button
			if (Buttons[i].Action = TextInput) and not ReadingTexT() then
			begin
				ReadFont := FontNamed('MenuText');
				StartReadingTextWithText(Buttons[i].Payload.Str, ColorBlack, 20, ReadFont, Round(Pos.x) + 10, Round(Pos.y + GetPadding(28, Extents.h)));
			end

			//volume button
			else if (Buttons[i].Payload.Kind = Volume) and (Extents.w <> 0) then
			begin
				vol := (MouseX() - Pos.x) / (Extents.w);
				SetMusicVolume(vol);
			end

			//all other buttons
			else if not (Buttons[i].Payload.Kind = None) then
			begin
				Buttons[i].Clicked := True;

				//end any text reading if another button is clicked
				if ReadingText() then
				begin
					EndReadingText();
				end;
			end;
		end;
	end;
end;

//Handles Player input to the menu module
//HandleMenuInput(ButtonArray, String)
procedure HandleMenuInput(var Buttons: ButtonArray; var Playername: String);
begin
	ProcessEvents();

	//Write Player Text Input when they finish inputting
	if KeyTyped(ReturnKey) then
	begin
		PlayerName := EndReadingText();
		WriteTextToButtonPayload(Buttons);
	end;

	//Button clicking
	if MouseClicked(LeftButton) then
	begin
		ButtonActionOnClick(Buttons, MousePosition());
	end;
end;

//////////////////
// Setup Menu Data objects
//////////////////

//Read Menu Buttons from file
//ReadMenuButtons(TextFile, ButtonArray);
procedure ReadMenuButtons(var MenuFile: TextFile; var Buttons: ButtonArray);
var
	count, i: Integer;
	line: String;
begin
	ReadLn(MenuFile, line);
	if (line = 'Buttons') then
	begin			
		ReadLn(MenuFile, count);
		SetLength(Buttons, count);
		for i:=0 to High(Buttons) do
		begin
			ReadLn(MenuFile, Buttons[i].ButtonName);
			ReadLn(MenuFile, Buttons[i].Pos.x);
			ReadLn(MenuFile, Buttons[i].Pos.y);
			ReadLn(MenuFile, Buttons[i].Extents.w);
			ReadLn(MenuFile, Buttons[i].Extents.h);
			Buttons[i].Color[0] := ColorGrey;
			Buttons[i].Color[1] := ColorWhite;
			ReadLn(MenuFile, Buttons[i].Action);
			ReadLn(MenuFile, Buttons[i].Payload.Kind);
			ReadLn(MenuFile, Buttons[i].Payload.Str);
		end;
	end;
end;

//Read array of text box entities from file
//ReadMenuTextBoxes(TextFile, TextBoxArray)
procedure ReadMenuTextBoxes(var MenuFile: TextFile; var TextBoxes: TextBoxArray);
var
	count, i: Integer;
	line: String;
begin
	ReadLn(MenuFile, line);
	if (line = 'TextBoxes') then
	begin
		ReadLn(MenuFile, count);
		SetLength(TextBoxes, count);
		for i:=0 to High(TextBoxes) do
		begin
			ReadLn(MenuFile, TextBoxes[i].Text);
			ReadLn(MenuFile, TextBoxes[i].Pos.x);
			ReadLn(MenuFile, TextBoxes[i].Pos.y);
			TextBoxes[i].Color := ColorWhite;
		end;
	end;
end;

//Read menu name from file
//ReadMenuName(TextFile, String)
procedure ReadMenuName(var MenuFile: TextFile; var MenuName: String);
var
	line: String;
begin	
	ReadLn(MenuFile, line);
	if (line = 'MenuName') then
	begin
		ReadLn(MenuFile, MenuName);
	end;
end;

//reads and returns a menu entity from file
//ReadMenuFromFile(TextFile, MenuType): Menu
function ReadMenuFromFile(var MenuFile: TextFile; const MenuKind: MenuType): Menu;
begin
	ReadLn(MenuFile, Result.MenuKind);
	if (MenuKind = Result.MenuKind) then
	begin
		ReadMenuName(MenuFile, Result.MenuName);
		ReadMenuTextBoxes(MenuFile, Result.TextBoxes);
		ReadMenuButtons(MenuFile, Result.Buttons);
	end
	else WriteLn('ReadMenuFromFile() Wrong menutype');
end;

//returns the path and filename of the specified menutype for reading and writing
//GetMenuFilename(MenuType): String;
function GetMenuFilename(const MenuKind: MenuType): String;
var
	line, SubDir: String;
begin
	Line := GetEnumName(TypeInfo(MenuType), Ord(MenuKind));
	SubDir := 'dat\menus\';
	result := Concat(SubDir, Line, '.dat');
end;

//setup the textfile for reading and returning a Menu entity
//GetMenuFromFile(MenuType): Menu
function GetMenuFromFile(const MenuKind: MenuType): Menu;
var
	MenuFile: TextFile;
	Filename: String;
begin
	Filename := GetMenuFilename(MenuKind);
	if FileExists(PathToResource(Filename)) then
	begin
		try			
			WriteLn('Reading ', PathToResource(FileName));
			AssignFile(MenuFile, PathToResource(Filename));
			Reset(MenuFile);		
			Result := ReadMenuFromFile(MenuFile, MenuKind);
		except
			WriteLn('File Handling Error');
		end;
		CloseFile(MenuFile);
	end
	else
	begin
		WriteLn('File does not exist');
	end;
end;

//Main controller for setting up menudata
//SetupMenuData(MenuArray, MenuPtr)
procedure SetupMenuData(var Menus: MenuArray);
var
	i: Integer;
begin
	//Setup the menu entities
	SetLength(Menus, ord(High(MenuType)) + 1);
	for i:=0 to High(Menus) do
	begin
		Menus[i] := GetMenuFromFile(MenuType(i));
	end;
end;

//Create starry background for the menus
//SetupMenuBackground(BackgroundData)
procedure SetupMenuBackground(var Background: BackgroundData);
begin
	Background.Stars := CreateStars(WINDOW_WIDTH, WINDOW_HEIGHT, 0.5);	
	Background.TwinkleCoolDown := CreateTimer();
	StartTimer(Background.TwinkleCooldown);
end;

//returns the player name as saved within the menu data struct
//GetPlayerName(MenuArray): String
function GetPlayerName(const Menus: MenuArray): String;
var
	i: Integer;
begin
	for i:=0 to High(Menus) do
	begin
		if (Menus[i].MenuKind = Options) then
		begin
			Result := GetButtonNamedPayload(Menus[i].Buttons, 'PlayerName');
			Exit;
		end;
	end;
end;

//menu debugging
//DebugMenus(MenuArray)
procedure DebugMenus(var Menus: MenuArray);
begin
	//reload menu from file
	if KeyTyped(RKey) then
	begin
		SetupMenuData(Menus);
	end;
end;

//////////////////
// Write menu data back to file
//////////////////

//Writes menu button data to file
//WriteMenuButtons(TextFile, ButtonArray)
procedure WriteMenuButtons(var MenuFile: TextFile; const Buttons: ButtonArray);
var
	i: Integer;
begin
	WriteLn(MenuFile, 'Buttons');
	WriteLn(MenuFile, Length(Buttons));

	for i:=0 to High(Buttons) do
	begin
		WriteLn(MenuFile, Buttons[i].ButtonName);
		WriteLn(MenuFile, Buttons[i].Pos.x);
		WriteLn(MenuFile, Buttons[i].Pos.y);
		WriteLn(MenuFile, Buttons[i].Extents.w);
		WriteLn(MenuFile, Buttons[i].Extents.h);
		WriteLn(MenuFile, Buttons[i].Action);
		WriteLn(MenuFile, Buttons[i].Payload.Kind);
		WriteLn(MenuFile, Buttons[i].Payload.Str);
	end;
end;

//writes menu textbox data to file
//WriteMenuTextBoxes(TextFile, TextBoxArray)
procedure WriteMenuTextBoxes(var MenuFile: TextFile; const TextBoxes: TextBoxArray);
var
	i: Integer;
begin
	WriteLn(MenuFile, 'TextBoxes');
	WriteLn(MenuFile, Length(TextBoxes));

	for i:=0 to High(TextBoxes) do
	begin		
		WriteLn(MenuFile, TextBoxes[i].Text);
		WriteLn(MenuFile, TextBoxes[i].Pos.x);
		WriteLn(MenuFile, TextBoxes[i].Pos.y);
	end;
end;

//Writes menu data to file
//WriteLinesToFile(TextFile, Menu)
procedure WriteLinesToFile(var MenuFile: TextFile; const Menu: Menu);
begin
	WriteLn(MenuFile, Menu.MenuKind);
	WriteLn(MenuFile, 'MenuName');
	WriteLn(MenuFile, Menu.Menuname);

	WriteMenuTextBoxes(MenuFile, Menu.TextBoxes);
	WriteMenuButtons(MenuFile, Menu.Buttons);
end;

//initialise the textfile and write the menu to it
//WriteMenuToFile(Menu)
procedure WriteMenuToFile(const Menu: Menu);
var
	Filename: String;
	MenuFile: TextFile;
begin
	Filename := GetMenuFilename(Menu.MenuKind);
	if FileExists(PathToResource(Filename)) then
	begin
		try
			WriteLn('Writing to ', PathToResource(FileName));
			AssignFile(MenuFile, PathToResource(Filename));
			ReWrite(MenuFile);
			WriteLinesToFile(MenuFile, Menu);
		except
			WriteLn('WriteMenuToFile() File Handling Error');
		end;
		CloseFile(MenuFile);
	end;
end;

//parent controller for writing menu data to text file
//UpdateMenuFiles(MenuArray)
procedure UpdateMenuFiles(const Menus: MenuArray);
var
	i: Integer;
begin
	for i:=0 to High(Menus) do
	begin
		WriteMenuToFile(Menus[i]);
	end;	
end;

//////////////////
// Main()
//////////////////

procedure Main();
var
	MenuModule: MenuData;
	GameInterface: GameInterfaceData;
	HighScore: Boolean;
	PlayerName: String;
begin
	OpenAudio();
	SetMusicVolume(0.4);
	OpenGraphicsWindow('WormHole', WINDOW_WIDTH, WINDOW_HEIGHT);
	LoadResourceBundleNamed('menu_module', 'menu_bundle.txt', true);
	PlayMusic('MenuMusic');

	SetupMenuData(MenuModule.Menus);
	SetupMenuBackground(MenuModule.Background);
	PlayerName := GetPlayerName(MenuModule.Menus);
	ChangeCurrentMenu(Root, MenuModule.CurrentMenu, MenuModule.Menus);

	repeat
		ClearScreen(ColorBlack);
		HandleMenuInput(MenuModule.CurrentMenu^.Buttons, PlayerName);
		HandleMenuState(MenuModule, GameInterface.Send);

		////
		//Call Main Game block
		//
		if (CheckGameStart(MenuModule.CurrentMenu^.Buttons)) then
		begin
			//start maingame();
			ReleaseAllTimers();
			StopMusic();
			ReleaseResourceBundle('menu_module');
			GameInterface.Receive := MainGameInterface(GameInterface.Send);
			
			//After game procedures
			StopMusic();
			LoadResourceBundleNamed('menu_module', 'menu_bundle.txt', true);
			PlayMusic('MenuMusic');
			MoveCameraTo(0,0);
			SetupMenuBackground(MenuModule.Background);

			//skip score screens if the player closed the window
			if not WindowCloseRequested() then
			begin
				//manage highscores
				HighScore := IsHighScore(GetMenuOfType(MenuModule.Menus, HighScores), GameInterface.Receive.Score);
				if HighScore then
				begin
					UpdateHighScores(GetMenuOfType(MenuModule.Menus, HighScores), GameInterface.Receive.Score, PlayerName);
				end;

				//dynamically build and display score screen
				ChangeCurrentMenu(ScoreScreen, MenuModule.CurrentMenu, MenuModule.Menus);
				MenuModule.CurrentMenu^.TextBoxes := CreateScoreScreen(GameInterface.Receive, HighScore);
			end;
		end;
		//
		//End Main Game block
		////

		DrawMenu(MenuModule.CurrentMenu^, MenuModule.Background);
		//DebugMenus(MenuModule.Menus);
		RefreshScreen(FPS);
	until WindowCloseRequested() or QuitBtnClicked(MenuModule.CurrentMenu^.Buttons);

	UpdateMenuFiles(MenuModule.Menus);

	CloseAudio();
	ReleaseAllResources();
end;

begin
	Main();
end.