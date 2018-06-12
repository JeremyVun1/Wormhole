Unit WH_Types_Menu;

//////////
//Public
//////////
Interface

uses SwinGame, WH_Types_Shared;

type
	MenuType = (Root, Select, Options, HighScores, Help, ScoreScreen);
	ButtonAction = (Quit, Save, NavTo, Play, TextInput);
	PayloadType = (MenuKind, Difficulty, ShipClass, Volume, Text, RotationControl, None);

	MenuPtr = ^Menu;

	PayloadData = record
		Kind: PayloadType;
		Str: String;
	end;

	ColorData = record
		Font: LongWord;
		Fill: LongWord;
		Border: LongWord;		
	end;

	ButtonData = record
		ButtonName: String;
		Pos: Point2D;
		Extents: Size;
		Action: ButtonAction;
		Payload: PayloadData;
		Clicked: Boolean;
		Highlighted: Boolean;
		Color: ColorData;
	end;

	TextBoxData = record
		Text: String;
		Pos: Point2D;
		Color: ColorData;
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

//////////
//Private
//////////
Implementation

end.