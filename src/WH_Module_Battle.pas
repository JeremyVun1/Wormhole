unit WH_Module_Battle;

interface

uses SysUtils, SwinGame, sgBackEndTypes, WH_Factory_Battle, WH_Settings, WH_Factory_Shared, WH_Utility_Battle, WH_Utility_Shared, WH_Types_Battle, WH_Types_Shared, WH_Types_Interface;

//Call Game Battle Module
//MainGame(DifficultyType, ShipType): ReceiveData
function MainGame(const Send: SendData): ReceiveData;

implementation

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

//Draws cursor icon
//DrawMouseCursor(Size)
procedure DrawMouseCursor(const Width: Single; const ControlKind: ShipControlType);
var
	MousePos: Point2D;
	CursorShape: LinesArray;
begin
	if (ControlKind = Mouse) then
	begin
		MousePos := ToWorld(MousePosition());
		AddCross(CursorShape, MousePos, Width/3);
		DrawShape(CursorShape, ColorYellow);
	end;
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

//Draws Wormhole spawn timer countdown text for NPC spawning
//DrawSpawnTimer(Timer, Boolean, DifficultyType)
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
procedure DrawPlayerUI(const Ship: ShipData; const UI: UIData; const Inventory: InventoryArray; const Score: Integer; ControlKind: ShipControlType);
begin
	DrawHealthBar(Ship.IsAlive, Ship.Powerup.Invincible.Switch, Ship.Health, Ship.ShipKind, UI.HealthContainer);
	DrawInventory(Inventory, UI);
	DrawPlayerBuffs(Ship.Powerup, UI);
	DrawMouseCursor(Ship.Extents.w, ControlKind);
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

//Draw the array of Tools
//DrawToolArray(ToolDataArray)
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

//Draw the List of Debris
//DrawDebrisList(DebrisListArray)
procedure DrawDebrisList(const DebrisList: DebrisListArray);
var
	i: Integer;
begin
	for i:=0 to High(DebrisList) do
	begin
		DrawShape(DebrisList[i].Shape, DebrisList[i].Color);
	end;
end;

//Draw the List of Loot
//DrawLootList(LootdataArray)
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

//Overlay for debugging Ships
//DebugShip(Ship)
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

//Draw the NPC Tier array of Ships
//DrawNPCTierShips(ShipDataArray)
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
procedure DrawGame(const PlayerData: Player; const NPCData: NPCTierArray; const LevelData: Level; const Send: SendData);
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
	
	DrawPlayerUI(PlayerData.Ship, PlayerData.UI, PlayerData.Inventory, LevelData.Score, Send.ShipControl);	
	DrawWormholeUI(LevelData.Wormhole, PlayerData.UI, LevelData.SpawnTimer, Send.Difficulty);

	//
	//DEBUG
	//DebugShip(PlayerData.Ship);
end;

//////////////////
// MainGame() Update Position procedures
//////////////////

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

		//rotate ship heading
		Ship.Move.Heading := RotateVector(Ship.Move.Heading, theta);

		//Rotate ship geometry
		RotationAnchor := Ship.Move.Pos;
		RotateShape(Ship.Shape, theta, RotationAnchor.x, RotationAnchor.y);

		//rotate ship tools and anchorpoints
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

//Releases all timer resources from the ship entity
//ReleaseShipTimers(ShipData)
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

//Handles Collisions between ammo and ships
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

//Handles Collisions between ammo list arrays and ships
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

//Handles collisions between NPC and Player Ships
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

//Handles Collisions between NPC Ship array and Player Ship
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

//Handles Collisions between Ammo and wormhole entities
//AmmoWormholecollisions(AmmoListArray, WormholeData, NumberPopupPtr, Boolean)
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
		HumanPlayer: color := ColorAqua;
		Computer: color := ColorRed;
		else
		begin
			WriteLn('KillShip() Invalid OwnerType');
			color := ColorRed;
		end;
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

//Mouse control rotation
//MouseRotation(MovementModel)
procedure MouseRotation(var Move: MovementModel);
var
	TargetPos: Point2D;
	TargetVec: Vector;
begin
	//Translate mouse point into relative point to ship position
	TargetPos := ToWorld(MousePosition());
	TargetPos.x := TargetPos.x - Move.Pos.x;
	TargetPos.y := TargetPos.y - Move.Pos.y;

	TargetVec := UnitVector(VectorToPoint(TargetPos));
	Move.TargetHeading := UnitVector(TargetVec);
end;

//Keyboard control rotation
//KeyboardRotation(MovementModel)
procedure KeyboardRotation(var Move: MovementModel; const ControlMap: ControlMapData);
begin
	if RotateLeftInput(ControlMap) and not RotateRightInput(ControlMap) then
	begin
		Move.TargetHeading := RotateVector(Move.Heading, -1 * Move.TurnRate);
	end;
	if RotateRightInput(ControlMap) and not RotateLeftInput(ControlMap) then
	begin
		Move.TargetHeading := RotateVector(Move.Heading, Move.TurnRate);
	end;
end;

//Handler for Rotation control based on Control Type
//HandleRotationInput(MovementModel, ShipControlType)
procedure HandleRotationInput(var Move: MovementModel; const ControlMap: ControlMapData);
begin
	case ControlMap.ControlKind of
		Mouse: MouseRotation(Move);
		Keyboard: KeyboardRotation(Move, ControlMap);
		else WriteLn('HandleRotationInput() - Invalid ControlType');
	end;
end;

//Handles Player movement inputs
//HandleMovementInput(MovementModel, array of Emitter)
procedure HandleMovementInput(var Move: MovementModel; var Emitter: array of EmitterData; const ControlMap: ControlMapData);
var	
	StrafeHeading: Vector;
begin
	//forward and backwards
	if AccelForwardInput(ControlMap) and not AccelBackwardInput(ControlMap) then
	begin
		Move.vel.x += Move.Heading.x * Move.Accel;
		Move.vel.y += Move.Heading.y * Move.Accel;
		ActivateEmittersNamed(Emitter, 'Thruster');
	end;
	if AccelBackwardInput(ControlMap) and not AccelForwardInput(ControlMap) then
	begin
		Move.vel.x += -Move.Heading.x * Move.Accel * Move.ReverseMod;
		Move.vel.y += -Move.Heading.y * Move.Accel * Move.ReverseMod;
		ActivateEmittersNamed(Emitter, 'Thruster');
	end;

	//strafe left and right
	//get strafe heading variable to help find actual strafe vectors
	StrafeHeading := VectorTo(0,0);
	if StrafeLeftInput(ControlMap) and not StrafeRightInput(ControlMap) then
	begin
		StrafeHeading := RotateVector(Move.Heading, -PI/2);
		Move.vel.x += StrafeHeading.x * Move.Accel * Move.StrafeMod;
		Move.vel.y += StrafeHeading.y * Move.Accel * Move.StrafeMod;
		ActivateEmittersNamed(Emitter, 'Thruster');
	end;
	if StrafeRightInput(ControlMap) and not StrafeLeftInput(ControlMap) then
	begin
		StrafeHeading := RotateVector(Move.Heading, PI/2);
		Move.vel.x += StrafeHeading.x * Move.Accel * Move.StrafeMod;
		Move.vel.y += StrafeHeading.y * Move.Accel * Move.StrafeMod;
		ActivateEmittersNamed(Emitter, 'Thruster');
	end;

	//Rotate left and right
	HandleRotationInput(Move, ControlMap);
end;

//handles player action inputs like item activation and tool weapon firing
//HandleActionInput(array of ToolData, InventoryArray, BuffData)
procedure HandleActionInput(var Tools: array of ToolData; var InvItems: InventoryArray; const Powerup: BuffData; const ControlMap: ControlMapData);
var
	i: Integer;
begin
	//Fire Tool Weapons
	if FireBallisticInput(ControlMap) then
	begin
		for i:=0 to High(Tools) do
		begin
			if (Tools[i].AmmoKind = Ballistic) then
			begin
				Tools[i].CoolDown.Switch := True;
			end;
		end;
	end;

	//Activate BuffData
	if ActivatePowerupInput(ControlMap) then
	begin
		for i:=0 to High(InvItems) do
		begin
			if CanActivateItem(InvItems[i], Powerup) then
			begin
				InvItems[i].Activated := True;
				Break;
			end;
		end;
	end;
end;

//Handles player input to the game state
//HandleGameInput(ShipData, InventoryArray);
procedure HandleGameInput(var Ship: ShipData; var Inventory: InventoryArray; const ControlMap: ControlMapData);
begin
	ProcessEvents();

	if (Ship.IsAlive) then
	begin
		HandleMovementInput(Ship.Move, Ship.Emitter, ControlMap);
		HandleActionInput(Ship.Tool, Inventory, Ship.Powerup, ControlMap);
	end;
end;

//////////////////
// MainGame() Setup Game Procedures
//////////////////

procedure SetupControlMap(var ControlMap: ControlMapData; const ShipControl: ShipControlType);
begin
	ControlMap := GetControlMap(ShipControl);
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
var
	BaseColor: LongWord;
begin
	BaseColor := GetOwnerColor(PlayerColor);
	Ship := CreateShip(ShipKind, BaseColor, GetStartingPosition(), HumanPlayer);
	AssertShipPointers(Ship);

	//player specific values placeholder
	Ship.Move.Accel *= PLAYER_SPEED_BUFF;
	Ship.Move.MaxVel *= PLAYER_SPEED_BUFF;
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

//Setup level data
//SetupLevel(Level, DifficultyType);
procedure SetupLevel(var LevelData: Level; const Difficulty: DifficultyType);
begin
	//Setup the starry background
	LevelData.Background := CreateBackground(PLAY_WIDTH, PLAY_HEIGHT);

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
procedure SetupGame(var GameModule: Game; const Send: SendData);
begin
	Randomize;
	LoadDefaultColors();

	SetupLevel(GameModule.LevelData, Send.Difficulty);
	SetupPlayer(GameModule.PlayerData, Send.ShipClass);
	SetupNPCData(GameModule.NPCData, Send.Difficulty);

	SetupControlMap(GameModule.ControlMap, Send.ShipControl);
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

function MainGame(const Send: SendData): ReceiveData;
var
	GameModule: Game;
	GameTime, EndDelay: Timer;
	EndMainGame: Boolean = False;
	Win: Boolean = False;
begin
	LoadResourceBundleNamed('game_module', 'game_bundle.txt', true);
	PlayMusic('GameMusic');
	HideMouse();

	try
		SetupGame(GameModule, Send);
		GameTime := CreateTimer();
		EndDelay := CreateTimer();
		StartTimer(GameTime);
	except
		WriteLn('Error setting up game');
		Exit;
	end;

	repeat
		ClearScreen(ColorBlack);

		HandleGameInput(GameModule.PlayerData.Ship, GameModule.PlayerData.Inventory, GameModule.ControlMap);
		HandleGameState(GameModule.PlayerData, GameModule.LevelData, GameModule.NPCData, Send.Difficulty);
		HandleNPCSpawning(GameModule.NPCData, Send.Difficulty, GameModule.LevelData.SpawnTimer);
		UpdateGamePositions(GameModule, Send.Difficulty);
		DrawGame(GameModule.PlayerData, GameModule.NPCData, GameModule.LevelData, Send);

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
	ShowMouse();
end;

end.