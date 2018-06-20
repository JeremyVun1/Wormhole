unit WH_Utility_Menu;

//////////
//public
//////////
interface

uses WH_Types_Menu, WH_Types_Shared, WH_Settings, SysUtils, SwinGame;

//returns whether the user has clicked the quit button or not
//QuitBtnClicked(MenuType, ButtonArray): Boolean
function QuitBtnClicked(const Buttons: ButtonArray): Boolean;

//translates point values from a relative value (percentage of window width and height) to an x,y point
//PercentToPosPoint(Point2D): Point2D
function PercentToPosPoint(Point: Point2D): Point2D;

//translates size values from relative value (percentage of window width and height) to a w, h size
//PercentToSizeExtent(Size): Size;
function PercentToSizeExtent(Extents: Size): Size;

//returns whether the passed in point is within the specified x, y, width, height box dimensions
//PointWithinBox(Point2D, Point2D, Size): Boolean
function PointWithinBox(Point: Point2D; BoxPos: Point2D; BoxSize: Size): Boolean;

//returns a pointer to the menu of the specified menukind
//GetMenuOfType(MenuArray, MenuType): MenuPtr
function GetMenuOfType(const Menus: MenuArray; const MenuKind: MenuType): MenuPtr;

//returns whether the score is high enough to be a high score
//IsHighScore(MenuPtr, Integer): Boolean
function IsHighScore(HighScoreMenu: MenuPtr; Score: Integer): Boolean;

//returns a padding ammount for centering things within containers nicely
//GetPadding(Single, Single): Single
function GetPadding(TextSize, BoxSize: Single): Single;

//Draws a button with the passed in attributes
//DrawButton(String, Point2D, Size, LongWord, LongWord)
procedure DrawButton(const Text: String; const Pos: Point2D; const Extents: Size; const FontColor, FillColor, BorderColor: LongWord);

//returns the payload string of the button with the specified button name
//GetButtonNamedPayload(ButtonArray, String)
function GetButtonNamedPayload(const Buttons: ButtonArray; const ButtonName: String): String;

//adds a textbox to the end of the array
//AddTextBox(TextBoxArray, String, Point2D, LongWord): TextBoxArray
function AddTextBox(TextBoxes: TextBoxArray; const Text: String; const Pos: Point2D; const Color: LongWord): TextBoxArray;

//Changes the current menu pointer to the menu of the passed in type
//ChangeCurrentMenu(MenuType, MenuPtr, MenuArray)
procedure ChangeCurrentMenu(MenuKind: MenuType; var CurrentMenu: MenuPtr; const Menus: MenuArray);

//clears the Highlights of all buttons of the specified Payload type group
//ClearBtnGroupHighlights(ButtonArray, PpayloadType): ButtonArray
function ClearBtnGroupHighlights(Buttons: ButtonArray; Kind: PayloadType): ButtonArray;

//inserts a string into the textbox array at the specified index
//InsertTextBoxAt(TextBoxArray, Integer, String);
procedure InsertTextBoxAt(var TextBoxes: TextBoxArray; const Index: Integer; const Str: String);

//Writes text input from player into the button payload
//WriteTextToButtonPayload(ButtonArray)
procedure WriteTextToButtonPayload(var Buttons: ButtonArray);

//returns the player name as saved within the menu data struct
//GetPlayerName(MenuArray): String
function GetPlayerName(const Menus: MenuArray): String;

//////////
//private
//////////
implementation

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

function PercentToPosPoint(Point: Point2D): Point2D;
begin
	Result.x := Point.x * WINDOW_WIDTH;
	Result.y := Point.y * WINDOW_HEIGHT;
end;

function PercentToSizeExtent(Extents: Size): Size;
begin
	Result.w := Extents.w * WINDOW_WIDTH;
	Result.h := Extents.h * WINDOW_HEIGHT;
end;

function PointWithinBox(Point: Point2D; BoxPos: Point2D; BoxSize: Size): Boolean;
begin
	result := False;
	if (Point.x > BoxPos.x) and (Point.x < (BoxPos.x + BoxSize.w)) and (Point.y > BoxPos.y) and (Point.y < (BoxPos.y + BoxSize.h)) then
	begin
		result := True;
	end;
end;

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

function GetPadding(TextSize, BoxSize: Single): Single;
begin
	Result := (BoxSize - TextSize) / 2;
end;

procedure DrawButton(const Text: String; const Pos: Point2D; const Extents: Size; const FontColor, FillColor, BorderColor: LongWord);
var
	TextWidth: Single;
begin
	FillRectangle(FillColor, Pos.x, Pos.y, Extents.w, Extents.h);
	DrawRectangle(BorderColor, Pos.x, Pos.y, Extents.w, Extents.h);
	TextWidth := Length(Text) * 10;
	DrawText(Text, FontColor, 'ButtonText', Pos.x + GetPadding(TextWidth, Extents.w), Pos.y + Extents.h/6);
end;

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

function AddTextBox(TextBoxes: TextBoxArray; const Text: String; const Pos: Point2D; const Color: LongWord): TextBoxArray;
var
	f: Integer;
begin
	SetLength(TextBoxes, Length(TextBoxes)+1);
	f := High(TextBoxes);

	TextBoxes[f].Text := Text;
	TextBoxes[f].Pos := Pos;
	TextBoxes[f].Color.Font := Color;

	Result := TextBoxes;
end;

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

function ClearBtnGroupHighlights(Buttons: ButtonArray; Kind: PayloadType): ButtonArray;
var
	i: Integer;
begin
	for i:=0 to High(Buttons) do
	begin
		if (Buttons[i].Payload.Kind = Kind) then
			Buttons[i].Selected := False;
	end;

	Result := Buttons;
end;

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

end.