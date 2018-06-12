unit WH_Factory_Shared;

///////////
//Public
///////////
interface

uses SwinGame, WH_Types_Shared, WH_Settings, WH_Utility_Battle;

//builds and returns an array of background stars
//CreateBackground(): BackgroundData
function CreateBackground(w, h: Single; OffsetOpt: Single = 0): BackgroundData;


///////////
//Private
///////////
implementation

function CreateStars(w, h: Single; const OffsetOpt: Single): StarArray;
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

function CreateBackground(w, h: Single; OffsetOpt: Single = 0): BackgroundData;
begin	
	Result.Stars := CreateStars(w, h, OffsetOpt);
	Result.TwinkleCoolDown := CreateTimer();
	StartTimer(Result.TwinkleCooldown);
end;

end.