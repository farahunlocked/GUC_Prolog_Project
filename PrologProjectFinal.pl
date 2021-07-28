
%________________________________Knowledge Database_____________________________________________

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).



%___________________________________possibleSubset/2________________________________________
sub0(List, List).
sub0(List, Rest) :-
   sub1(List, Rest).

sub1([_|Tail], Rest) :-
   sub0(Tail, Rest).
sub1([Head|Tail], [Head|Rest]) :-
   sub1(Tail, Rest).

possibleSubset(L,R):-
	sub0(L,Z),
	permutation(Z,R).


%________________________________choosePreferences/2________________________________________

choosePreferences(Prefs, ChosenPreferences):-
	findActivity(Prefs,A),
	sub0(A,S),
	insertActivity(Prefs,S,ResultPrefs),
	sub0(ResultPrefs,ChosenPreferences).

findActivity([],[]).
findActivity([H|_],A):-
	H=activity(A).
findActivity([H|T],A):-
	H\=activity(_),
	findActivity(T,A).

insertActivity([],_,[]).
insertActivity(List,A,Result):-
	List=[H|T],
	H=activity(_),
	Result=[H2|T2],
	H2=activity(A),
	insertActivity(T,A,T2).
insertActivity(List,A,Result):-
	List=[H|T],
	H\=activity(_),
	Result=[H|T2],
	insertActivity(T,A,T2).





%________________________________preferenceSatisfaction/4__________________________________________

preferenceSatisfaction(Offer, Customer, ChosenPrefs, S):-
	ChosenPrefs=[H|T],
	preferenceSatisfaction(Offer, Customer, [H|T], S, 0).

preferenceSatisfaction(_, _,[], Acc, Acc).


preferenceSatisfaction(Offer, Customer, [H|T], S, Acc):-
	H=activity(A),
	Offer=offer(_, Activities, _, _, _, _, _, _),
	calcAct(Customer, A, Activities, 0, Act),
	Newacc is Act+Acc,
	preferenceSatisfaction(Offer, Customer,T, S,Newacc).



preferenceSatisfaction(Offer, Customer, [H|T], S, Acc):-
	offerAccommodation(Offer,X),
	H=accommodation(X),
	customerPreferredAccommodation(Customer,X,R2),
	Newacc is R2+Acc,
	preferenceSatisfaction(Offer, Customer,T, S,Newacc).


preferenceSatisfaction(Offer, Customer, [H|T], S, Acc):-

	H=means(X),
	offerMean(Offer,M),
	X=M,
	customerPreferredMean(Customer,M,R3),
	Newacc is R3+Acc,
	preferenceSatisfaction(Offer, Customer,T, S,Newacc).

preferenceSatisfaction(Offer,Customer,[H|T],S, Acc):-
	H\=means(_),
	H\=accommodation(_),
	H\=activity(_),
	preferenceSatisfaction(Offer, Customer,T, S,Acc).


calcAct(_, [], _, Accum, Accum).

calcAct(Customer, [X|Z], OffActList,Sum1, Accum):-
	member(X,OffActList),
	customerPreferredActivity(Customer,X,R1),
	Sum2 is Sum1+R1,
	calcAct(Customer,Z,OffActList,Sum2, Accum).

calcAct(Customer, [X|Z], OffActList,Sum1, Accum):-
	\+member(X,OffActList),
	calcAct(Customer,Z, OffActList, Sum1, Accum).

getOneActivity([H|_],H).
getOneActivity([_|T],R):-
	getOneActivity(T,R).

%_______________________________overlapPeriod/2____________________________________________

overlapPeriod(P1,P2):-
	P1=period(D1,D2),
	P2=period(D3,_),
	D3@=<D2,
	D3@>=D1.

overlapPeriod(P1,P2):-
	P1=period(D1,D2),
	P2=period(D3,D4),
	D3@=<D2,
	D3@=<D1,
	D4@=<D2,
	D4@>=D1.
%___________________________________getOffer/2_______________________________________________


getOffer(ChosenPrefs,Offer):-
	ChosenPrefs=[H|T],
	member(means(_),[H|T]),
	\+member(accomodation(_),[H|T]),
	getOffer(ChosenPrefs,Draft,Offer),
	Draft=offer(D,A,C,V1,V2,P,W,NG),
	Offer=offer(D,A,C,V1,V2,P,W,NG).

getOffer(ChosenPrefs,Offer):-
	ChosenPrefs=[H|T],
	member(accomodation(_),[H|T]),
	\+member(means(_),[H|T]),
	getOffer(ChosenPrefs,Draft,Offer),
	Draft=offer(D,A,C,V1,V2,P,W,NG),
	Offer=offer(D,A,C,V1,V2,P,W,NG).

getOffer(ChosenPrefs,Offer):-
	ChosenPrefs=[H|T],
	\+member(accomodation(_),[H|T]),
	\+member(means(_),[H|T]),
	getOffer(ChosenPrefs,Draft,Offer),
	Draft=offer(D,A,C,_,_,P,_,_),
	offerMean(offer(D,A,C,V1,V2,P,W,NG),_),
	Offer=offer(D,A,C,V1,V2,P,W,NG).

getOffer(ChosenPrefs,Offer):-
	ChosenPrefs=[H|T],
	member(accomodation(_),[H|T]),
	member(means(_),[H|T]),
	getOffer(ChosenPrefs,Draft,Offer),
	Draft=offer(D,A,C,V1,V2,P,W,NG),
	Offer=offer(D,A,C,V1,V2,P,W,NG).



getOffer([],Draft,Draft).


getOffer(ChosenPrefs,Draft,Offer):-
	ChosenPrefs=[H|T],
	H=activity(A),
	offerMean(offer(_,X,_,_,_,_,_,_),_),
	possibleSubset(X,A),
	Draft=offer(_,X,_,_,_,_,_,_),
	getOffer(T,Draft,Offer).

getOffer(ChosenPrefs,Draft,Offer):-
	ChosenPrefs=[H|T],
	H=dest(D),
	offerMean(offer(D,_,_,_,_,_,_,_),_),
	Draft=offer(D,_,_,_,_,_,_,_),
	getOffer(T,Draft,Offer).

getOffer(ChosenPrefs,Draft,Offer):-
	ChosenPrefs=[H|T],
	H=budget(B),
	offerMean(offer(_,_,C,_,_,_,_,_),_),
	B>=C,
	Draft=offer(_,_,C,_,_,_,_,_),
	getOffer(T,Draft,Offer).

getOffer(ChosenPrefs,Draft,Offer):-
	ChosenPrefs=[H|T],
	H=period(_,_),
	offerMean(offer(_,_,_,_,_,P,_,_),_),
	overlapPeriod(H,P),
	Draft=offer(_,_,_,_,_,P,_,_),
	getOffer(T,Draft,Offer).

getOffer(ChosenPrefs,Draft,Offer):-
	ChosenPrefs=[H|T],
	H=accommodation(A),
	offerAccommodation(Draft,A),
	Draft1=Draft,
	getOffer(T,Draft1,Offer).

getOffer(ChosenPrefs,Draft,Offer):-
	ChosenPrefs=[H|T],
	H=means(M),
	offerMean(Draft,M),
	Draft1=Draft,
	getOffer(T,Draft1,Offer).

getOffer(ChosenPrefs,Draft,Offer):-
	H\=dest(_),
	H\=budget(_),
	H\=period(_,_),
	H\=activity(_),
	H\=means(_),
	H\=accommodation(_),
	ChosenPrefs=[H|T],
	getOffer(T,Draft,Offer).








%__________________________________________________recommendOfferForCustomer/3_________________________________________


recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
	choosePreferences(Prefs, ChosenPrefs),
	getOffer(ChosenPrefs,O).


%___________________________________________________recommendOffer/4__________________________________________

recommendOffer(_,[],_,[]).
recommendOffer(Customers, PreferenceList, Offer, CustomersChosen):-
   recommendOfferHelper(Customers,PreferenceList,PreferenceList,Offer,CustomersChosen).



%to save the orginal Pref list as some might need it.
recommendOfferHelper(_,[],_,_,[]).
recommendOfferHelper(Customers,PreferenceList,FullPreferenceList,Offer,CustomersChosen):-
    PreferenceList=[H|T],
    recommendOfferForCustomer(H, _, Offer),
    possibleCustomers(Offer,Customers,FullPreferenceList,AllpossibleCust),
   getFinalCust(Offer,AllpossibleCust,0,CustomersChosen),
   recommendOfferHelper(Customers,T,FullPreferenceList,_,_).


% Brings Allcustomers tht match offer nd orders them from highest satisf
% to lowest.
possibleCustomers(_,[],[],[]).
possibleCustomers(Offer,CustomerList,PreferenceList,[MaxCustomer|Result]):-
   CustomerList\=[],
   PreferenceList\=[],
   maxSat(Offer,CustomerList,PreferenceList,0,_,_,MaxCustPrefs,MaxCustomer),
   removeItem(CustomerList,MaxCustomer,Newcustomerlist),
   removeItem(PreferenceList, MaxCustPrefs, NewPreferenceList),
   possibleCustomers(Offer,Newcustomerlist,NewPreferenceList,Result).


% removes item froom list.
removeItem([],_,[]).

removeItem([H|T],X,[H|Result]):-
   H\=X,
   removeItem(T,X,Result).

removeItem([X|T],X, Result):-
    removeItem(T,X,Result).



% gets one cust with highest sat at a time( we update the lists in
% another method to get second highest nd third and so on.

maxSat(_,[],[],_,MaxCustomer, MaxPrefs, MaxPrefs, MaxCustomer).

maxSat(Offer,CustomerList,PreferenceList,MaxsoFar, _, _, MaxCustPrefs, MaxCustomer ):-

   CustomerList=[H|T],
   PreferenceList=[H2|T2],
   preferenceSatisfaction(Offer, H, H2, S),
   S>MaxsoFar,
   maxSat(Offer,T, T2, S, H, H2, MaxCustPrefs, MaxCustomer).

maxSat(Offer,CustomerList,PreferenceList,MaxsoFar, MaxCustomersoFar, MaxCustPrefsSoFar,MaxCustPrefs, MaxCustomer):-
   CustomerList=[H|T],
   PreferenceList=[H2|T2],
   preferenceSatisfaction(Offer, H, H2, S),
   S=<MaxsoFar,
   maxSat(Offer,T, T2, MaxsoFar,MaxCustomersoFar, MaxCustPrefsSoFar, MaxCustPrefs,MaxCustomer).



% selects from the ordered possible customer to the needed amount of
% guests.(only takes up to N guests from possible cust list. Counts num
% of guests.

getFinalCust(_,[],_,[]).
getFinalCust(Offer,AllpossibleCust,Count,[H|Result]):-

   AllpossibleCust=[H|T],
   Offer=offer(_,_,_,_,_,_,_,N),
   Count<N,
   Newcount is Count+1,
   getFinalCust(Offer,T,Newcount,Result).

getFinalCust(Offer,AllpossibleCust,Count,R):-
   AllpossibleCust\=[],
   Offer=offer(_,_,_,_,_,_,_,N),
   Count>=N,
   getFinalCust(Offer,[],Count,R).


%_____________Extension Assessment_______________________


getAllActivities(L):-
	setof(X,Y^Z^customerPreferredActivity(Y,X,Z),L).


mostPreferredActivity(C,A):-
	getAllActivities(ActivityList),
	helper(C,A,ActivityList,0,Bestactsofar).

helper(_,A,[],_,A).
helper(C,A,[H|T],MostRating,Bestactsofar):-
                customerPreferredActivity(C,H,Rating),
                Rating>=MostRating,
                helper(C,A,T,Rating,H).
helper(C,A,[H|T],MostRating,Bestactsofar):-
       customerPreferredActivity(C,H,Rating),
       Rating<MostRating,
       helper(C,A,T,MostRating,Bestactsofar).
helper(C,A,[H|T],MostRating,Bestactsofar):-
   \+customerPreferredActivity(C,H,Rating),
   helper(C,A,T,MostRating,Bestactsofar).


