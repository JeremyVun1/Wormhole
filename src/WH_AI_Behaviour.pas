unit WH_AI_Behaviour;

////////////
// Public
////////////
interface

uses SwinGame, Math, WH_Types_Battle, WH_Utility_Battle, WH_Settings;

//NPC randomly moves around the map
//ErraticBehaviour(MovementModel)
procedure ErraticBehaviour(var Move: MovementModel);

//NPC chases after the plaayer
//ChaseBehaviour(MovementModel, Point2D)
procedure ChaseBehaviour(var Move: MovementModel; const TargetPos: Point2D);

//Finds the nearest target enemy ship to lock onto
//SeekTarget(Point2D, ShipList, TargetingData, Ownertype)
procedure SeekTarget(const Pos: Point2D; const ShipList: ShipListData; var Targeting: TargetingData; const Owner: OwnerType);

//Set appropriate target heading to chase the target ship
//ChaseTarget(MovementModel, TargetingData)
procedure ChaseTarget(var Move: MovementModel; var Targeting: TargetingData);

////////////
// Private
////////////
implementation

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

function FindNearestShip(Pos: Point2D; PShip: ShipPtr): ShipPtr; Overload;
begin
	if PShip^.IsAlive then Result := PShip
	else Result := nil;
end;

function FindNearestShip(Pos: Point2D; NPCData: NPCDataPtr): ShipPtr; Overload;
var
	i, j: Integer;
	minDist, Dist: Single;
	TargetShip: ShipPtr;
begin
	minDist := max(PLAY_WIDTH, PLAY_HEIGHT); //seed minimum dist
	Result := nil;

	for i:=0 to High(NPCData^) do
	begin
		for j:=0 to High(NPCData^[i].Ships) do
		begin
			TargetShip := @NPCData^[i].Ships[j];
			Dist := PointPointDistance(Pos, TargetShip^.Move.Pos);
			if (Dist < MISSILE_LOCK_RANGE) and (Dist < minDist) then
			begin
				minDist := Dist;
				Result := TargetShip;
			end;
		end;
	end;
end;

function CanSeekNewTarget(Seeking: Boolean): Boolean;
begin
	Result := not Seeking;
end;

procedure SeekTarget(const Pos: Point2D; const ShipList: ShipListData; var Targeting: TargetingData; const Owner: OwnerType);
begin
	if CanSeekNewTarget(Targeting.Seeking) then
	begin
		try
			//find nearest target
			case Owner of
				HumanPlayer: Targeting.Target := FindNearestShip(Pos, ShipList.NPC);
				Computer: Targeting.Target := FindNearestShip(Pos, ShipList.Player);
			end;

			if (Targeting.Target = nil) then
			begin
				Targeting.Seeking := False
			end
			else Targeting.Seeking := True;
		except
			WriteLn('SeekTarget() - Error');
		end;
	end;
end;

procedure ChaseTarget(var Move: MovementModel; var Targeting: TargetingData);
begin
	try
		if (Targeting.Target = nil) or CanSeekNewTarget(Targeting.Seeking) or not (Targeting.Target^.IsAlive) then 
		begin
			Targeting.Seeking := False;
			Targeting.Target := nil;
			Exit;
		end
		else ChaseBehaviour(Move, Targeting.Target^.Move.Pos);
	except
		WriteLn('ChaseTarget() - invalid target');
	end;
end;

end.