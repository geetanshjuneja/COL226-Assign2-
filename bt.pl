ibt(empty).
ibt(node(N, L, R)):- integer(N), ibt(L), ibt(R).

% Predicate where N contains number of integer-labelled nodes in BT.
size(ibt(empty), 0). % Base Case
% Size of BT = Size of Left BT + Size of Right BT  
size(BT, N) :- BT = ibt(node(_,L,R)), size(ibt(L),N2), size(ibt(R),N3), N is N2 + N3 + 1. 

% Predicate where N contains the height of BT.
height(ibt(empty),0).% Base Case
% Height of BT = max(height(LBT),height(RBT)) + 1.
height(BT, N) :- BT = ibt(node(_,L,R)), height(ibt(L),N1), height(ibt(R),N2), N is max(N1, N2) + 1.

% Predicate to find preorder of a BT.
preorder(ibt(empty), []). %Base case
% First process the rootnode then move to left subtree and then right subtree.
preorder(BT, L) :- BT = ibt(node(N,Left,Right)), preorder(ibt(Left),L1), preorder(ibt(Right),L2), append(L1, L2, L3), L = [N|L3].

% Predicate to find inorder of a BT.
inorder(ibt(empty),[]). %Base case
% First process the left subtree then rootnode and then move to right subtree.
inorder(BT, L) :- BT = ibt(node(N,Left,Right)), inorder(ibt(Left), L1), inorder(ibt(Right), L2), append(L1, [N], L3), append(L3, L2, L).

% Predicate to find postorder of a BT.
postorder(ibt(empty),[]). %Base case
% First process the left subtree then right subtree and then process rootnode.
postorder(BT, L) :- BT = ibt(node(N,Left,Right)), postorder(ibt(Left), L1), postorder(ibt(Right), L2), append(L1, L2, L3), append(L3, [N], L).

% Predicate to find eulerTour of a BT.
eulerTour(BT, L) :- BT = ibt(node(N, empty, empty)), L = [N,N,N]. % Base Case
% If Right Subtree of a node is empty.
eulerTour(BT, L) :- BT = ibt(node(N, Temp, empty)), eulerTour(ibt(Temp), L1), L2 = [N|L1], append(L2, [N,N], L).
% If Left Subtree of a node is empty.
eulerTour(BT, L) :- BT = ibt(node(N, empty, Temp)), eulerTour(ibt(Temp), L1), L2 = [N|[N|L1]], append(L2, [N], L).
eulerTour(BT, L) :-  BT = ibt(node(N,Left,Right)), eulerTour(ibt(Left), L1), eulerTour(ibt(Right), L2), append([N|L1], [N|L2], L3), append(L3,[N],L).

% Predicate used to remove numbers from euler tour list to get post,in,preorder
rem(X,Y) :- Y is mod(X,3).
remove([], A, _, A).
remove([H|T], A, D, R) :- count(T, H, X), rem(X,D), remove(T, [H|A], D, R); count(T, H, X), not(rem(X,D)), remove(T, A, D, R).
remove(X, Y, R) :- remove(X,[], Y, L), reverse(L,R).

% Predicate to count occurrences of a number in a List.
count([],_,0).
count([Head|Tail], Head, Y) :- count(Tail, Head, Y1), Y is Y1 + 1.
count([Head|Tail], X, Y) :- X=\=Head, count(Tail, X, Y). 

% Predicate to calculate postorder from euler tour by removing first two occurrences of a number.
postET(BT, L) :- eulerTour(BT, L1), remove(L1,0,L).

% Predicate to calculate inorder from euler tour by removing first and last occurrence of a number.
inEt(BT, L) :- eulerTour(BT, L1), remove(L1,1, L).

% Predicate to calculate preorder from euler tour by removing last two occurrences of a number.
preET(BT, L) :- eulerTour(BT, L1), remove(L1, 2, L).

check(X,Y) :- Z is abs(X-Y), (Z = 0; Z = 1).

% Predicate to check whether BT is balanced or not.
isBalanced(ibt(empty)). % Base case
% Starts from rootnode and checks  whether the height(LBT) - height(RBT) is 0 or 1 and recursively moves to the LBT and RBT. 
isBalanced(BT) :- BT = ibt(node(_,Left,Right)), height(ibt(Left),X), height(ibt(Right),Y), check(X, Y), isBalanced(ibt(Left)), isBalanced(ibt(Right)).

% Predicate checks whether the BT follows the property of BST(<) or not.
checkOrder([_]).
checkOrder([Head1,Head2|Tail]) :- Head1 < Head2, checkOrder([Head2|Tail]).

isBST(BT) :- inorder(BT, L), checkOrder(L).


find_median([X|T], X, L1, L2, T) :- reverse(L2, L1).

find_median([H|T], X, L1, L2, L3) :- find_median(T, X, L1, [H|L2], L3).

makeAVL([], empty).
makeAVL(L, Node) :-  proper_length(L, Z), Y is Z // 2, nth0(Y, L, X), find_median(L, X, L1, [], L2), makeAVL(L1, Node1), makeAVL(L2, Node2), Node = node(X, Node1, Node2).


makeBST(L, BST) :- sort(L, L1), makeAVL(L1, N), BST = ibt(N).

% If after insertion balanced BST is not mandatory then following predicate for insertion is more efficient.
/*insert(N,ibt(empty),ibt(node(N,empty,empty))).
insert(N, BST1, BST2) :- BST1 = ibt(node(N1,Left,Right)), N < N1, insert(N, ibt(Left), ibt(NewLeft)), BST2 = ibt(node(N1,NewLeft,Right)).
insert(N, BST1, BST2) :- BST1 = ibt(node(N1,Left,Right)), N > N1, insert(N, ibt(Right), ibt(NewRight)), BST2 = ibt(node(N1,Left,NewRight)).
*/
insert(N, BST1, BST2) :- inorder(BST1, L1), L = [N|L1], makeBST(L, BST2).


% If after deletion balanced BST is not mandatory then following predicate for insertion is more efficient.
/*find_successor([N,N1|_], N, N1).
find_successor([_|T], N, N1) :- find_successor(T, N, N1).

successor(N, BST, N1) :- inorder(BST, List), find_successor(List, N, N1).

delete(N, ibt(node(N,empty,empty)), ibt(empty)).
delete(N, (ibt(node(N,T,empty)); ibt(node(N,empty,T))), ibt(T)).
delete(N, ibt(node(N,L,R)), BST2) :- successor(N, ibt(node(N,L,R)), N1), delete(N1, ibt(node(N,L,R)), ibt(node(N,L1,R1))), BST2 = ibt(node(N1,L1,R1)). 
delete(N, BST1, BST2) :- BST1 = ibt(node(N1,L,R)), N < N1, delete(N, ibt(L), ibt(NL)), BST2 = ibt(node(N1, NL, R)). 
delete(N, BST1, BST2) :- BST1 = ibt(node(N1,L,R)), N > N1, delete(N, ibt(R), ibt(NR)), BST2 = ibt(node(N1, L, NR)). */

delete(N, BST1, BST2) :- lookup(N, BST1), inorder(BST1, L1), select(N, L1, L), makeBST(L, BST2).

% Predicate to check whether N is present in BST or not using BST property(<).
lookup(N, ibt(node(N,_,_))).
lookup(N, BST) :- BST = ibt(node(N1,Left,_)), N < N1, lookup(N, ibt(Left)).
lookup(N, BST) :- BST = ibt(node(N1,_,Right)), N > N1, lookup(N, ibt(Right)).

% Finds the preorder of a BT using stack.
stack(empty, [], List, List).
stack(empty, [node(_,_,empty)|Stack_list], List, Result) :- stack(empty, Stack_list, List, Result).
stack(empty, [node(_,_,R)|Stack_list], List, Result) :- stack(R, Stack_list, List, Result). 
stack(node(N,L,R), Stack_list, List, Result) :- stack(L, [node(N,L,R)|Stack_list], [N|List], Result).
trPreorder(BT, L) :- BT = ibt(node(N,L2,R)), stack(L2,[node(N,L2,R)],[N],L1), reverse(L1, L).

% Finds the inorder of a BT using stack.
stack_forIN(empty, [], List, List).
stack_forIN(empty, [node(N,_,empty)|Stack_list], List, Result) :- stack_forIN(empty, Stack_list, [N|List], Result).
stack_forIN(empty, [node(N,_,R)|Stack_list], List, Result) :- stack_forIN(R, Stack_list, [N|List], Result).
stack_forIN(node(N,L,R), Stack_list, List, Result) :- stack_forIN(L, [node(N,L,R)|Stack_list], List, Result).
trInorder(BT, L) :- BT = ibt(node(N,L2,R)), stack_forIN(L2,[node(N,L2,R)],[],L1), reverse(L1, L).

% Finds the postorder of a BT using mainstack and childstack.
mainStack(empty, [], [], List, List).
mainStack(empty, [node(N,_,_)|Stack_list], [], List, Result) :- mainStack(empty, Stack_list, [], [N|List], Result).
mainStack(empty, [node(N,_,_)|Stack_list], [R|RChild_stack], List, Result) :-  mainStack(empty, Stack_list, [R|RChild_stack], [N|List], Result).
mainStack(empty, [node(N,L,R)|Stack_list], [R|RChild_stack], List, Result) :- mainStack(R, [node(N,L,R)|Stack_list], RChild_stack, List, Result).
mainStack(node(N,L,empty), Stack_list, RChild_stack, List, Result) :- mainStack(L, [node(N,L,empty)|Stack_list], RChild_stack, List, Result).
mainStack(node(N,L,R), Stack_list, RChild_stack, List, Result) :- mainStack(L, [node(N,L,R)|Stack_list], [R|RChild_stack], List, Result).
trPostorder(BT, L) :- BT = ibt(node(N,L2,R)), mainStack(L2,[node(N,L2,R)], [R], [], L1), reverse(L1, L).
trPostorder(BT, L) :- BT = ibt(node(N,L2,empty)), mainStack(L2,[node(N,L2,empty)], [], [], L1), reverse(L1, L).

% Converts the BT to string.
toString(ibt(empty), "()").
toString(BT, S) :- BT = ibt(node(N,Left,Right)), toString(ibt(Left),S1), toString(ibt(Right), S2), atom_string(N, S3), string_concat("(", S3, S4), string_concat(S4, ",", S5), string_concat(S5, S1, S6), string_concat(S6, ",", S7), string_concat(S7, S2, S8), string_concat(S8, ")", S).

% ibt(node(1,node(5,node(4,node(2,empty,empty),node(5,empty,empty)),empty),empty))
% ibt(node(3, node(2, empty, empty), node(1, empty, empty)))
% ibt(node(3, node(1, empty, empty), node(4, empty, empty)))