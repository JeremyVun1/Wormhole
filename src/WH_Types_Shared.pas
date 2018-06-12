Unit WH_Types_Shared;

//////////
//Public
//////////
Interface

uses SwinGame;

type
	ShipType = (LightShip, MediumShip, HeavyShip);
	DifficultyType = (Novice, Intermediate, Expert);

	Size = record
		w: Single;
		h: Single;
	end;

	Star = record
		Pos: Point2D;
		Size: Single;
	end;

	StarArray = array of Star;

	BackgroundData = record
		Stars: StarArray;
		TwinkleCoolDown: Timer;
	end;

//////////
//Private
//////////
Implementation

end.