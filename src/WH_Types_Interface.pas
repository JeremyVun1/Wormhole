unit WH_Types_Interface;

///////////
//Public
///////////
interface

uses WH_Types_Shared;

type
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

implementation

end.