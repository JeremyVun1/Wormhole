unit WH_Types_Battle;

interface

uses sgBackEndTypes, SwinGame, WH_Types_Shared;

type
	OwnerType = (HumanPlayer, Computer);
	OwnerColorType = (PlayerColor, Tier1Color, Tier2Color, Tier3Color);
	AmmoType = (Ballistic, Missile);
	ItemType = (Invincible, Speed, Damage);
	NumberPopupType = (DamagePopup, PointsPopup);
	AIType = (Static, Chase, Erratic);

	ShipColorArray = array[0..3] of LongWord;
	Point2DArray = array of Point2D;
	AnchorPtr = ^Point2D;
	AnchorArrayPtr = ^Point2DArray;
	HeadingPtr = ^Vector;
	NPCDataPtr = ^NPCTierArray;
	ShipPtr = ^ShipData;

	MouseButtonMap = record
		FireBallistic: MouseButton;
		FireMissile: MouseButton;
		ActivatePowerup: MouseButton;
	end;

	KeyCodeMap = record
		RotateLeft: array[0..1] of KeyCode;
		RotateRight: array[0..1] of KeyCode;
		StrafeLeft: array[0..1] of KeyCode;
		StrafeRight: array[0..1] of KeyCode;
		AccelForward: array[0..1] of KeyCode;
		AccelBackward: array[0..1] of KeyCode;
		FireBallistic: array[0..1] of KeyCode;
		FireMissile: array[0..1] of KeyCode;
		ActivatePowerup: array[0..1] of KeyCode;
	end;

	ControlMapData = record
		ControlKind: ShipControlType;
		Keyboard: KeyCodeMap;
		Mouse: MouseButtonMap;
	end;

	TimerPackage = record
		Clock: Timer;
		SubClock: Timer;
		Switch: Boolean;
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

	TargetingData = record
		Target: ShipPtr;
		Seeking: Boolean;
	end;

	AmmoData = record
		Owner: Ownertype;
		IsAlive: Boolean;
		AmmoKind: AmmoType;
		Move: MovementModel;
		Damage: Single;
		Expiry: Timer;
		Targeting: TargetingData;
		Shape: LinesArray;
		Color: LongWord;
		AnchorPoint: Point2DArray;
		Emitter: EmitterDataArray;
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

	ShipListData = record
		Player: ShipPtr;
		NPC: NPCDataPtr;		
	end;

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

	Level = record
		Background: BackgroundData;
		DebrisList: DebrisListArray;
		LootList: LootDataArray;
		Wormhole: WormholeData;
		Score: Integer;
		SpawnTimer: Timer;
	end;

	Game = record		
		ControlMap: ControlMapData;
		LevelData: Level;
		PlayerData: Player;
		NPCData: NPCTierArray;
	end;

implementation

end.