distance_3d(Point1, X2, Y2, Z2, Distance) :-
    point(Point1, X1, Y1, Z1),
    Distance is sqrt((X2 - X1) ** 2 + (Y2 - Y1) ** 2 + (Z2 - Z1) ** 2).

% Distance between two points in 3D space
distance_3d(Point1, Point2, Distance) :-
    point(Point1, X1, Y1, Z1),
    point(Point2, X2, Y2, Z2),
    Distance is sqrt((X2 - X1) ** 2 + (Y2 - Y1) ** 2 + (Z2 - Z1) ** 2).

% Distance between a point and a line segment in 3D space
distance_point_segment_3d(Point, Start, End, Distance) :-
    point(End, XE, YE, ZE),
    point(Start, XS, YS, ZS),
    point(Point, XP, YP, ZP),
    distance_3d(Start, End, SegmentLength),
    distance_3d(Start, Point, DistanceStart),
    distance_3d(End, Point, DistanceEnd),
    ProjX is XS + ((XE - XS) / SegmentLength) * (XP - XS),
    ProjY is YS + ((YE - YS) / SegmentLength) * (YP - YS),
    ProjZ is ZS + ((ZE - ZS) / SegmentLength) * (ZP - ZS),
    distance_3d(Start, ProjX, ProjY, ProjZ, ProjToStart),
    distance_3d(End, ProjX, ProjY, ProjZ, ProjToEnd),
    (
     ProjToStart > SegmentLength ->  
      Distance = DistanceEnd;
     (   ProjToEnd > SegmentLength, ProjToStart =< SegmentLength)->  
      Distance = DistanceStart;
        distance_3d(Point, ProjX, ProjY, ProjZ, Distance)
    ).

list_min([L|Ls], Min) :- foldl(min_, Ls, L, Min).
min_(A, B, Min) :- Min is min(A, B).

% Distance between two line segments in 3D space
distance_segment_segment_3d(Point1, Point2, Point3, Point4, Distance) :-
    distance_point_segment_3d(Point1, Point3, Point4, Distance1),
    distance_point_segment_3d(Point2, Point3, Point4, Distance2),
    distance_point_segment_3d(Point3, Point1, Point2, Distance3),
    distance_point_segment_3d(Point4, Point1, Point2, Distance4),
    list_min([Distance1, Distance2, Distance3, Distance4], Distance).


% Rules for calculating the angle between three points in 3D
angle(Point1, Point2, Point3, Angle) :-
    % Get the coordinates of the points
    point(Point1, X1, Y1, Z1),
    point(Point2, X2, Y2, Z2),
    point(Point3, X3, Y3, Z3),
    % Calculate the vectors
    Vector1X is X1 - X2,
    Vector1Y is Y1 - Y2,
    Vector1Z is Z1 - Z2,
    Vector2X is X3 - X2,
    Vector2Y is Y3 - Y2,
    Vector2Z is Z3 - Z2,
    % Calculate the lengths of the vectors
    Length1 is sqrt(Vector1X * Vector1X + Vector1Y * Vector1Y + Vector1Z * Vector1Z),
    Length2 is sqrt(Vector2X * Vector2X + Vector2Y * Vector2Y + Vector2Z * Vector2Z),
    % Calculate the dot product
    DotProduct is Vector1X * Vector2X + Vector1Y * Vector2Y + Vector1Z * Vector2Z,
    % Calculate the angle between the vectors
    TempCosAngle is DotProduct / (Length1 * Length2),
    clamp_cos(TempCosAngle, CosAngle),
    Angle is acos(CosAngle) * 180 / pi.

% Predicate to limit the angle within the range from A to B
clamp_cos(CosAngle, ClampedAngle) :-
    CosAngle >= -1,
    CosAngle =< 1,
    ClampedAngle is CosAngle.
clamp_cos(CosAngle, ClampedAngle) :-
    CosAngle < -1,
    ClampedAngle is -1.
clamp_cos(CosAngle, ClampedAngle) :-
    CosAngle > 1,
    ClampedAngle is 1.

% Check the length of a phalanx
checkLength(Point1, Point2, FingerName) :-
    % Get the coordinates of the points
    point(Point1, X1, Y1, Z1),
    point(Point2, X2, Y2, Z2),
    % Calculate the distance between the points
    Distance is sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1) + (Z2 - Z1) * (Z2 - Z1)),
    % Check the length of the phalanx
    (\+ (Distance >= 2, Distance =< 7) ->
        format('Error in finger: ~w. Invalid phalanx length.~n', [FingerName]), fail ;
        true).

% Check the angle between phalanges
checkAngle(Point1, Point2, Point3, MinAngle, MaxAngle, FingerName) :-
    angle(Point1, Point2, Point3, Angle),
    (\+ (Angle >= MinAngle, Angle =< MaxAngle) ->
        format('Error in finger: ~w. Invalid angle between phalanges.~n', [FingerName]), fail ;
        true).

checkFinger(Number, Name, MinAngle, MaxAngle):-
    finger(Number, P0, P1, P2, P3),
    % Check the length of the thumb phalanges
    checkLength(P0, P1, Name),
    checkLength(P1, P2, Name),
    checkLength(P2, P3, Name),
    % Check the angles of the thumb phalanges
    checkAngle(P0, P1, P2, MinAngle, MaxAngle, Name),
    checkAngle(P1, P2, P3, MinAngle, MaxAngle, Name).


checkPalm:-
    checkAngle(1, 0, 4, 10, 40, "Thumb and Index Finger"),
    checkAngle(4, 0, 8, 10, 40, "Index Finger and Middle Finger"),
    checkAngle(8, 0, 12, 10, 40, "Middle Finger and Ring Finger"),
    checkAngle(12, 0, 16, 10, 40, "Ring Finger and Little Finger").

checkPhalangsCrossing(P10, P11, P20, P21, Width1, Width2, Name):-
    distance_segment_segment_3d(P10, P11, P20, P21, Dist),
    (\+ (Dist >= ((Width1 + Width2) / 2)) ->
        format('Error in fingers: ~w. Fingers are crossing.~n', [Name]), fail;
        true).

checkFingerCrossing(F1, F2, Name):-
    fingerWidth(F1, W1),
    fingerWidth(F2, W2),
    
    finger(F1, P10, P11, P12, P13),
    finger(F2, P20, P21, P22, P23),
    
    checkPhalangsCrossing(P10, P11, P20, P21, W1, W2, Name),
    checkPhalangsCrossing(P10, P11, P21, P22, W1, W2, Name),
    checkPhalangsCrossing(P10, P11, P22, P23, W1, W2, Name),
    checkPhalangsCrossing(P11, P12, P20, P21, W1, W2, Name),
    checkPhalangsCrossing(P11, P12, P21, P22, W1, W2, Name),
    checkPhalangsCrossing(P11, P12, P22, P23, W1, W2, Name),
    checkPhalangsCrossing(P12, P13, P20, P21, W1, W2, Name),
    checkPhalangsCrossing(P12, P13, P21, P22, W1, W2, Name),
    checkPhalangsCrossing(P12, P13, P22, P23, W1, W2, Name), !.
    
% Check the entire hand
checkHand:-
    checkFinger(0, "Thumb", 150, 180),
    checkFinger(1, "Index finger", 150, 180),
    checkFinger(2, "Middle finger", 150, 180),
    checkFinger(3, "Ring finger", 150, 180),
    checkFinger(4, "Little finger", 150, 180),
    checkPalm,
    checkFingerCrossing(0, 1, "0, 1"),
    checkFingerCrossing(0, 2, "0, 2"),
    checkFingerCrossing(0, 3, "0, 3"),
    checkFingerCrossing(0, 4, "0, 4"),
    checkFingerCrossing(1, 2, "1, 2"),
    checkFingerCrossing(1, 3, "1, 3"),
    checkFingerCrossing(1, 4, "1, 4"),
    checkFingerCrossing(2, 3, "2, 3"),
    checkFingerCrossing(2, 4, "2, 4"),
    checkFingerCrossing(3, 4, "3, 4"), !.

% Points (coordinates)
% Starting point
point(0, 0, 0, 0).

% Thumb
point(1, 3, 3, 0).
point(2, 6, 6, 0).
point(3, 9, 9, 0).
point(20, 12, 12, 0).

% Index finger
point(4, 1, 3, 0).
point(5, 2, 6, 0).
point(6, 3, 9, 0).
point(7, 4, 12, 0).

% Middle finger
point(8, -1, 3, 0).
point(9, -2, 6, 0).
point(10, -3, 9, 1).
point(11, -4, 12, 2).

% Ring finger
point(12, -3, 3, 0).
point(13, -6, 6, 0).
point(14, -9, 9, 0).
point(15, -12, 12, 0).

% Little finger
point(16, -5, 3, 0).
point(17, -8, 6, 0).
point(18, -11, 9, 0).
point(19, -14, 12, 0).

finger(0, 1, 2, 3, 20).
finger(1, 4, 5, 6, 7).
finger(2, 8, 9, 10, 11).
finger(3, 12, 13, 14, 15).
finger(4, 16, 17, 18, 19).

fingerWidth(0, 0.5).
fingerWidth(1, 0.5).
fingerWidth(2, 0.1).
fingerWidth(3, 0.1).
fingerWidth(4, 0.1).
