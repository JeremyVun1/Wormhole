unit WH_Module_Menu;

//////////
//Public
//////////
interface

uses SysUtils, SwinGame, SgTypes, TypInfo, WH_Factory_Shared, WH_Module_Interface, WH_Types_Interface, WH_Types_Menu, WH_Types_Shared, WH_Settings, WH_Utility_Menu, WH_Utility_Shared;

//Start the Menu Module
//MainMenu()
procedure MainMenu();

//////////
//Private
//////////
implementation

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

//adds time text to the end of the array and returns it
//AddTimeText(TextBoxArray, Integer): TextBoxArray
function AddTimeText(TextBoxes: TextBoxArray; const Time: Integer): TextBoxArray;
var
	line: String;
begin
	line := Concat('You Survived for: ', IntToStr(Time), ' seconds');
	Result := AddTextBox(TextBoxes, line, PointAt(0.28, 0.6), ColorWhite);
end;

//adds score text to the end of the array and returns it
//AddScoreText(TextBoxArray, Integer): TextBoxArray
function AddScoreText(TextBoxes: TextBoxArray; const Score: Integer): TextBoxArray;
var
	line: String;
begin
	line := Concat('Your Score: ', IntToStr(Score));
	Result := AddTextBox(TextBoxes, line, PointAt(0.38, 0.5), ColorWhite);
end;

//adds and returns text indicating whether score is a highscore
//AddHighScoreText(TextBoxArray, Boolean): TextBoxArray
function AddHighScoreText(TextBoxes: TextBoxArray; const HighScore: Boolean): TextBoxArray;
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
//AddWinLoseText(TextBoxArray, Boolean): TextBoxArray
function AddWinLoseText(TextBoxes: TextBoxArray; const Win: Boolean): TextBoxArray;
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
	TextBoxes := AddWinLoseText(TextBoxes, Receive.Win);
	TextBoxes := AddHighScoreText(TextBoxes, HighScore);
	TextBoxes := AddScoreText(TextBoxes, Receive.Score);
	TextBoxes := AddTimeText(TextBoxes, Receive.Time);

	Result := TextBoxes;
end;


//validate that the required options are valid payload enumerations
//ValidateReqOptions(array of String): Boolean
function ValidateReqOptions(const ReqOpt: array of String): Boolean;
var
	i, j, validated: Integer;
begin
	validated := 0;

	for i:=0 to High(ReqOpt) do
	begin
		for j:=Ord(Low(PayloadType)) to Ord(High(PayloadType)) do
		begin
			if (GetEnumName(TypeInfo(PayloadType), j) = ReqOpt[i]) then
			begin
				validated += 1;
			end;
		end;
	end;

	if (validated = length(ReqOpt)) then
	begin
		result := true;
	end
	else result := false;
end;

//validation check whether all the save actiontype buttons have been selected (difficulty and ship class)
//OptionsSelected(ButtonArray, array of String): Boolean;
function OptionsSelected(const Buttons: ButtonArray; const ReqOptions: array of String): Boolean;
var
	i, j, SelectedCount, RequiredCount: Integer;
	PayloadKind: PayloadType;
begin
	Result := False;
	SelectedCount := 0;
	RequiredCount := Length(ReqOptions);

	if ValidateReqOptions(ReqOptions) then
	begin
		//
		for i:=0 to High(ReqOptions) do
		begin
			ReadStr(ReqOptions[i], PayloadKind);
			
			for j:=0 to High(Buttons) do
			begin
				if (Buttons[j].Payload.Kind = PayloadKind) and (Buttons[j].Highlighted) then
				begin
					SelectedCount += 1;
					Break;
				end;
			end;
		end;

		//check if all required options have been selected
		if (SelectedCount = RequiredCount) then
		begin
			result := true;	
		end
		else result := false;
	end
	else
	begin
		WriteLn('OptionsSeleced() - invalid options specified');
	end;
end;

//returns whether the player has requested to start the Root Game
//CheckGameStart(MenuType, ButtonArray): Boolean
function CheckGameStart(var Buttons: ButtonArray): Boolean;
var
	i: Integer;
	requiredOptions: array of String;
begin
	Result := False;
	for i:=0 to High(Buttons) do
	begin
		if (Buttons[i].Action = Play) and (Buttons[i].Clicked = True) then
		begin
			Buttons[i].Clicked := False;

			//build required options parameter record
			SetLength(requiredOptions, 2);
			requiredOptions[0] := GetEnumName(TypeInfo(PayloadType), Ord(Difficulty));
			requiredOptions[1] := GetEnumName(TypeInfo(PayloadType), Ord(ShipClass));

			//check that required options are selected
			if OptionsSelected(Buttons, requiredOptions) then
			begin
				Result := True;
			end
			else PlaySoundEffect('Error');
			Exit;
		end;
	end;
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

//Start the Menu Module
procedure MainMenu();
var
	MenuModule: MenuData;
	GameInterface: GameInterfaceData;
	HighScore: Boolean;
	PlayerName: String;
begin
	OpenAudio();
	SetMusicVolume(0.1);
	OpenGraphicsWindow('WormHole', WINDOW_WIDTH, WINDOW_HEIGHT);
	LoadResourceBundleNamed('menu_module', 'menu_bundle.txt', true);
	PlayMusic('MenuMusic');

	SetupMenuData(MenuModule.Menus);
	MenuModule.Background := CreateBackground(WINDOW_WIDTH, WINDOW_HEIGHT, 0.5);
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
			MenuModule.Background := CreateBackground(WINDOW_WIDTH, WINDOW_HEIGHT, 0.5);

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

end.