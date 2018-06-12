unit WH_Utility_Shared;

////////////
//Public
////////////
interface

uses WH_Types_Battle, WH_Types_Shared, WH_Settings, SwinGame, sgBackEndTypes;

//Add a Line segment at the specified coordinates to the LinesArray
//AddLine(LinesArray, Single, Single, Single, Sginle, Point2D)
procedure AddLine(var Shape: LinesArray; x1, y1, x2, y2: Single; const Pos: Point2D);

//Add a rectangle shape at the specified coordinates to the LinesArray
//AddRectangle(LinesArray, Single, Single, Single, Single, Point2D)
procedure AddRectangle(var Shape: LinesArray; x, y, w, h: Single; const Pos: Point2D);

//Add a triangle shape at the specified coordinates to the LinesArray
//AddTriangle(LinesArray, Single, Single, Single, Single, Point2D)
procedure AddTriangle(var Shape: LinesArray; x, y, w, h: Single; const Pos: Point2D);

//Returns integer representation of the sign(+-) of the input parameter
//GetSign(Single): Single
function GetSign(a: Single): Single;

//clamps x within -MaxNum to +MaxNum
//ClampNumber(Single, Single): Single
function ClampNumber(x, MaxNum: Single): Single;

//returns number of seconds that have elapsed for the passed in timer
//GetTimerSeconds(Timer): Single
function GetTimerSeconds(const Clock: Timer): Single;

//Turns the timer package on
//StartTimerPackage(TimerPackage)
procedure StartTimerPackage(var Package: TimerPackage);

//Turns the timer package off
//StopTimerPackage(TimerPackage)
procedure StopTimerPackage(var Package: TimerPackage);

//frees the timers in the timer package
//ReleaseTimerPackage(TimerPackage)
procedure ReleaseTimerPackage(var Package: TimerPackage);

//Draws LinesArray Shape
//DrawShape(LongWord, LinesArray)
procedure DrawShape(const Shape: LinesArray; const Color: LongWord);

//draws a shape that randomly alternates between two specified colors
//DrawTwinklingShape(LinesArray, LongWord, LongWord)
procedure DrawTwinklingShape(const Shape: LinesArray; Color1, Color2: LongWord);

//Draw the starry background
//DrawStars(StarArray)
procedure DrawStars(const Stars: StarArray);

//handler for drawing background data
//DrawBackground(BackgroundData)
procedure DrawBackground(const Background: BackgroundData);

//Manages star twinkling states
//HandleStarTwinkling(StarArray, Timer)
procedure HandleStarTwinkling(var Stars: StarArray; const TwinkleCoolDown: Timer);

//manages the starry level background
//HandleBackground(BackgroundData)
procedure HandleBackground(var Background: BackgroundData);

//translates color string into SwinGame 32bit rgba color codes
//RGBAColorCode(String): LongWord;
function RGBAColorCode(Color: String): LongWord;

//translates rgba 32 bit color code into a string
//ColorString(LongWord): String;
function ColorString(Color: LongWord): String;

////////////
//Private
///////////
implementation

procedure AddLine(var Shape: LinesArray; x1, y1, x2, y2: Single; const Pos: Point2D);
var
    f: Integer;
begin
	SetLength(Shape, Length(Shape)+1);
	f:=High(Shape);

	Shape[f].StartPoint := PointAt(Pos.x + x1, Pos.y + y1);
	Shape[f].EndPoint := PointAt(Pos.x + x2, Pos.y + y2);
end;

procedure AddRectangle(var Shape: LinesArray; x, y, w, h: Single; const Pos: Point2D);
begin
	AddLine(Shape, x, y, x+w, y, Pos); //top
	AddLine(Shape, x+w, y, x+w, y+h, Pos); //right
	AddLine(Shape, x+w, y+h, x, y+h, Pos); //bottom
	AddLine(Shape, x, y+h, x, y, Pos); //left
end;

procedure AddTriangle(var Shape: LinesArray; x, y, w, h: Single; const Pos: Point2D);
begin
	AddLine(Shape, x, y, x+w, y, Pos); //base
	AddLine(Shape, x, y, x+(w/2), y-h, Pos); //left diagonal
	AddLine(Shape, x+w, y, x+(w/2), y-h, Pos); //right diagonal
end;

function GetSign(a: Single): Single;
begin
	if (a < 0) then Result := -1
	else if (a > 0) then Result := 1
	else Result := 0;
end;

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

function GetTimerSeconds(const Clock: Timer): Single;
begin
	Result := TimerTicks(Clock)/1000;
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

procedure DrawShape(const Shape: LinesArray; const Color: LongWord);
var
	i: Integer;
begin
	for i:=0 to High(Shape) do
	begin
		DrawLine(Color, Shape[i]);
	end;
end;

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

procedure DrawBackground(const Background: BackgroundData);
begin
	DrawStars(Background.Stars);
end;

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

procedure HandleBackground(var Background: BackgroundData);
begin
	HandleStarTwinkling(Background.Stars, Background.TwinkleCooldown);
end;

function RGBAColorCode(Color: String): LongWord;
begin
	case Color of
		'Red': Result := ColorRed;
		'Crimson': Result := ColorCrimson;
		'Orange': Result := ColorOrange;
		'LightYellow': Result := ColorLightYellow;
		'Yellow': Result := ColorYellow;
		'Green': Result := ColorGreen;
		'BrightGreen': Result := ColorBrightGreen;
		'Cyan': Result := ColorCyan;
		'LightSteelBlue': Result := ColorLightSteelBlue;
		'Blue': Result := ColorBlue;
		'SkyBlue': Result := ColorSkyBlue;
		'Indigo': Result := ColorIndigo;
		'Violet': Result := ColorViolet;
		'BlueViolet': Result := ColorBlueViolet;
		'White': Result := ColorWhite;
		'Grey': Result := ColorGrey;
		'GhostWhite': Result := ColorGhostWhite;
		'Black': Result := ColorBlack;
		'Aqua': Result := ColorAqua;
		'AquaMarine': Result := ColorAquamarine;
		'Brown': Result := ColorBrown;
		'BurlyWood': Result := ColorBurlyWood;
		'Gold': Result := ColorGold;
		'Silver': Result := ColorSilver;
		'Purple': Result := ColorPurple;
		'Transparent': Result := ColorTransparent;
		else Result := ColorWhite;
	end;
end;

function ColorString(Color: LongWord): String;
begin
	if (Color = ColorRed) then Result := 'Red'
	else if (Color = ColorCrimson) then Result := 'Crimson'
	else if (Color = ColorOrange) then Result := 'Orange'
	else if (Color = ColorLightYellow) then Result := 'LightYellow'
	else if (Color = ColorYellow) then Result := 'Yellow'
	else if (Color = ColorGreen) then Result := 'Green'
	else if (Color = ColorBrightGreen) then Result := 'BrightGreen'
	else if (Color = ColorCyan) then Result := 'Cyan'
	else if (Color = ColorLightSteelBlue) then Result := 'LightSteelBlue'
	else if (Color = ColorBlue) then Result := 'Blue'
	else if (Color = ColorSkyBlue) then Result := 'SkyBlue'
	else if (Color = ColorIndigo) then Result := 'Indigo'
	else if (Color = ColorViolet) then Result := 'Violet'
	else if (Color = ColorBlueViolet) then Result := 'BlueViolet'
	else if (Color = ColorWhite) then Result := 'White'
	else if (Color = ColorGrey) then Result := 'Grey'
	else if (Color = ColorGhostWhite) then Result := 'GhostWhite'
	else if (Color = ColorBlack) then Result := 'Black'
	else if (Color = ColorAqua) then Result := 'Aqua'
	else if (Color = ColorAquamarine) then Result := 'AquaMarine'
	else if (Color = ColorBrown) then Result := 'Brown'
	else if (Color = ColorBurlyWood) then Result := 'BurlyWood'
	else if (Color = ColorGold) then Result := 'Gold'
	else if (Color = ColorSilver) then Result := 'Silver'
	else if (Color = ColorPurple) then Result := 'Purple'
	else if (Color = ColorTransparent) then Result := 'Transparent'
	else Result := 'White';
end;

end.