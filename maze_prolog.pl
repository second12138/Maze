% Group 18 - CSCI 3136 - Prolog Project - Dr. Norbert Zeh
% Yansong Li - B00755354 | Yichen Zhao - B00775330 | Le Wang - B00761974

:- initialization main.

main :-
    current_prolog_flag(argv, [InputFilename, OutputFilename]),
    mazeRead(InputFilename, Height, Width, Maze),
    mazeToGraph(Height, Width, Maze, Graph),
    searchPath(Height, Width, Graph, [(0, [0])], Path),
    resultSave(OutputFilename, Width, Path),
    halt.


% read maze from input file
mazeRead(InputFilename, Height, Width, Maze) :-
    open(InputFilename, read, InputStream, [type(binary)]),
    read_stream_to_codes(InputStream, Codes),
    append(Codes1, Codes2, Codes),
    append([Height1, Height2, Height3, Height4], [Width1, Width2, Width3, Width4], Codes1),
    Height is Height1 + Height2 * 256 + Height3 * 256 * 256 + Height4 * 256 * 256 * 256,
    Width is Width1 + Width2 * 256 + Width3 * 256 * 256 + Width4 * 256 * 256 * 256,
    codesToMaze(Codes2, Maze),
    close(InputStream).

% save the result to output file
resultSave(OutpuFilename, Width, Path) :-
    open(OutpuFilename, write, OutStream),
    resultSaveToStream(Path, Width, OutStream),
    close(OutStream). 

% path to output stream
resultSaveToStream(Path, Width, OutStream) :-
    (
        Path = [Index|TailPath] -> X is Index div Width,
                                   Y is Index mod Width,
                                   write(OutStream, X),
                                   write(OutStream,' '), 
                                   writeln(OutStream,Y),
                                   resultSaveToStream(TailPath, Width, OutStream)
        ; otherwise             -> true

    ).

% maze list to graph data structure
mazeToGraph(Height, Width, Maze, Graph) :-
    (
        Maze = []                                         -> Graph = []
        ; Maze = [[Index, _]|_], Height * Width =:= Index -> Graph = []
        ; otherwise                                       -> Maze = [HeadMaze|TailMaze],
                                                             toCell(Height, Width, HeadMaze, HeadGraph),
                                                             mazeToGraph(Height, Width, TailMaze, TailGraph),
                                                             Graph = [HeadGraph|TailGraph]
    ).


% search the path using dfs algorithm
searchPath(Height, Width, Maze, ExtendsList, Path) :-
    ExtendsList = [(Index, P)|TailExtendsList],
    (
        Index =:= Height * Width - 1 -> reverse(P, Path)
        ; otherwise                  -> getExtends(Index, Width, Maze, P, NewExtends),
                                        append(NewExtends, TailExtendsList, NewExtendsList),
                                        searchPath(Height, Width, Maze, NewExtendsList, Path)
    ).

% get the extends nodes from a node
getExtends(Index, Width, Graph, Path, Extends) :-
    Path = [CurrentNode|_],
    nth0(Index, Graph, Nexts),
    getUpperNodes(Index, Width, CurrentNode, Graph, UpperNodes),
    getLeftNodes(Index, CurrentNode, Graph, LeftNodes),
    append(Nexts, UpperNodes, Extends1),
    append(Extends1, LeftNodes, Extends2),
    deleteList(Extends2, [Index|Path], ExtendsNode),
    findall((Node, [Node|Path]), member(Node, ExtendsNode), Extends).

% get upper nodes
getUpperNodes(Index, Width, CurrentNode, Graph, UpperNodes) :-
    IndexWidth is Index - Width,
    (
        nth0(IndexWidth , Graph, Ux), 
        member(Index, Ux), 
        CurrentNode =\= Index - Width  -> E is Index - Width, UpperNodes = [E]
        ; otherwise                    -> UpperNodes = []
    ).

% get left nodes
getLeftNodes(Index, CurrentNode, Graph, LeftNodes) :-
    Index1 is Index - 1,
    (
        nth0(Index1 , Graph, Lx), 
        member(Index, Lx), 
        CurrentNode =\= Index - 1  -> E is Index - 1, LeftNodes = [E]
        ; otherwise                -> LeftNodes = []
    ).


% the element in maze list to cell in graph data structure
toCell(Height, Width, [Index, (A, B)], Cell) :-
    (
        (A, B) = (0, 0) -> (
                              (Index + 1) mod Width =:= 0, Index + Width >= Height * Width -> Cell = []
                              ; (Index + 1) mod Width =:= 0     -> E is Index + Width, Cell = [E]
                              ; Index + Width >= Height * Width -> E is Index + 1, Cell = [E]
                              ; otherwise                       -> E1 is Index + Width, E2 is Index + 1, Cell = [E1, E2]
                           );
        (A, B) = (0, 1) -> (
                              (Index + 1) mod Width =:= 0 -> Cell = []
                              ; otherwise                 -> E is Index + 1, Cell = [E]
                           );
        (A, B) = (1, 0) -> (
                              Index + Width >= Height * Width -> Cell = []
                              ; otherwise                     -> E is Index + Width, Cell = [E]
                           );
        otherwise       -> Cell = []
        
    ).


% number to maze list
codesToMaze(Codes, Maze) :-
    maplist(codeToBinaries, Codes, BinariesLists),
    concat(BinariesLists, BinariesList),
    binariesListToMaze(BinariesList, Maze, 0).


% binaryies list to maze list 
binariesListToMaze(BinariesList, Maze, N) :-
    (
       BinariesList = [] -> Maze = []
       ; otherwise       -> Next is N + 1,
                            BinariesList = [X,Y|TailBinariesList],
                            binariesListToMaze(TailBinariesList, TailMaze, Next),
                            Maze = [[N, (X, Y)]|TailMaze]

    ).
    
% number to binary list
codeToBinaries(Code, Binaries) :-
    codeToBinaries(Code, [], RevBinaries),
    reverse(Binaries, RevBinaries).
codeToBinaries(Code, BinariesAcc, Binaries) :-
    Mod is Code mod 2, 
    Div is Code div 2,
    length(BinariesAcc, Length),
    (
        Length < 8  -> codeToBinaries(Div, [Mod|BinariesAcc], Binaries)
        ; otherwise -> Binaries = BinariesAcc
    ).


% helper functions
concat(List, ConcatList) :- 
    (
        List = [Head|Tail] -> concat(Tail, TailConcatList),
                              append(Head, TailConcatList, ConcatList)
        ; otherwise        -> ConcatList = []
    ).

% delete the list
deleteList(List, DeleteList, Result) :-
    (
        DeleteList = [H|T] -> delete(List, H, NewList),
                              deleteList(NewList, T, Result)
        ; otherwise        -> Result = List
    ).
