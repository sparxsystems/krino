﻿using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Graphs;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.StateMachines
{
    public class MultiMachine<TState, TTrigger>
    {
        private DirectedGraph<TState, IRule<TTrigger>> myGraph;
        private Dictionary<TState, StateDefinition<TState>> myStates = new Dictionary<TState, StateDefinition<TState>>();
        private TransitImmediatelyRule<TTrigger> myImmediateTransitRule = new TransitImmediatelyRule<TTrigger>();

        private Tree<StateRecord<TState, TTrigger>> myMachineTrack = new Tree<StateRecord<TState, TTrigger>>(null);
        private List<Tree<StateRecord<TState, TTrigger>>> myActiveStates = new List<Tree<StateRecord<TState, TTrigger>>>();
        private List<Tree<StateRecord<TState, TTrigger>>> myUnhandledStates = new List<Tree<StateRecord<TState, TTrigger>>>();
        

        private IEqualityComparer<TState> myStateEqualityComparer;
        private IEqualityComparer<TTrigger> myTriggerEqualityComparer;


        public MultiMachine(IEqualityComparer<TState> stateComparer = null, IEqualityComparer<TTrigger> triggerComparer = null)
        {
            myStateEqualityComparer = stateComparer ?? EqualityComparer<TState>.Default;
            myTriggerEqualityComparer = triggerComparer ?? EqualityComparer<TTrigger>.Default;

            // Note: the edge is of type IRule which implements IEquatable, therefore the default comparer is ok to use for triggers.
            myGraph = new DirectedGraph<TState, IRule<TTrigger>>(stateComparer, null);

            DefaultTriggerRuleFactoryMethod = x => RuleMaker.Is(x, myTriggerEqualityComparer);
        }

        public Func<TTrigger, IRule<TTrigger>> DefaultTriggerRuleFactoryMethod { get; set; }


        public IEnumerable<TState> States => myGraph;

        public IEnumerable<Tree<StateRecord<TState, TTrigger>>> ActiveStateRecords => myActiveStates;

        public IEnumerable<Tree<StateRecord<TState, TTrigger>>> UnhandledStateRecords => myUnhandledStates;


        public void AddInitialState(TState state) => AddState(state, StateKind.Initial);

        public void AddState(TState state) => AddState(state, StateKind.Custom);

        public void AddFinalState(TState state) => AddState(state, StateKind.Final);


        public void AddInitialSubState(TState parent, TState subState) => AddSubState(parent, subState, StateKind.Initial);

        public void AddSubState(TState parent, TState subState) => AddSubState(parent, subState, StateKind.Custom);

        public void AddFinalSubState(TState parent, TState subState) => AddSubState(parent, subState, StateKind.Final);


        public void AddTransition(TState from, TState to) => AddTransition(from, to, myImmediateTransitRule);

        public void AddTransition(TState from, TState to, TTrigger trigger) => AddTransition(from, to, DefaultTriggerRuleFactoryMethod(trigger));

        public void AddTransition(TState from, TState to, IRule<TTrigger> triggerRule) => myGraph.AddEdge(from, to, triggerRule);


        public void Fire(TTrigger trigger)
        {
            var activeStates = myActiveStates.ToList();

            myActiveStates.Clear();
            myUnhandledStates.Clear();

            foreach (var activeState in activeStates)
            {
                var fromState = GetStateToContinue(activeState.Value.Definition);

                var triggersFromState = myGraph.GetEdgesGoingFrom(fromState.Value);
                var applicableTriggers = triggersFromState.Where(x => !x.Value.Equals(myImmediateTransitRule) && x.Value.Evaluate(trigger));
                if (applicableTriggers.Any())
                {
                    foreach (var edge in applicableTriggers)
                    {
                        if (myStates.TryGetValue(edge.To, out var toState))
                        {
                            var toStates = GetStatesToStart(toState);
                            foreach (var stateToContinue in toStates)
                            {
                                var newRecord = new Tree<StateRecord<TState, TTrigger>>(new StateRecord<TState, TTrigger>(stateToContinue, edge.Value, trigger));
                                activeState.Add(newRecord);

                                myActiveStates.Add(newRecord);
                            }
                        }
                    }
                }
                else
                {
                    activeState.Value.IsUnhandled = true;
                    activeState.Value.UnhandledTrigger = trigger;
                    myUnhandledStates.Add(activeState);
                }
            }

            FireImmediateTransitions();
        }

        public void Reset()
        {
            myMachineTrack.Clear();
            myActiveStates.Clear();
            myUnhandledStates.Clear();

            var allTopLevelInitialStates = myStates.Values.Where(x => x.StateKind == StateKind.Initial && !x.IsSubstate);

            // Note: initial state cannot have substates.
            var newRecords = allTopLevelInitialStates.Select(x => new Tree<StateRecord<TState, TTrigger>>(new StateRecord<TState, TTrigger>(x, null, default(TTrigger))));

            myActiveStates.AddRange(newRecords);

            FireImmediateTransitions();
        }

        private void AddState(TState state, StateKind stateKind)
        {
            myGraph.AddVertex(state);

            var newState = new StateDefinition<TState>(state, stateKind);
            myStates[state] = newState;
        }

        private void AddSubState(TState parent, TState subState, StateKind stateKind)
        {
            myGraph.AddVertex(subState);

            var newState = new StateDefinition<TState>(subState, stateKind)
            {
                Parent = parent,
            };
            myStates[subState] = newState;
        }

        private IEnumerable<StateDefinition<TState>> GetStatesToStart(StateDefinition<TState> state)
        {
            IEnumerable<StateDefinition<TState>> result;

            var initialSubStates = GetSubstates(state).Where(x => x.StateKind == StateKind.Initial);
            if (initialSubStates.Any())
            {
                result = initialSubStates;
            }
            else
            {
                result = new StateDefinition<TState>[] { state };
            }

            return result;
        }

        private StateDefinition<TState> GetStateToContinue(StateDefinition<TState> state)
        {
            StateDefinition<TState> result;
            
            if (state.IsSubstate && state.StateKind == StateKind.Final && myStates.TryGetValue(state.Parent, out var parent))
            {
                result = parent;
            }
            else
            {
                result = state;
            }

            return result;
        }

        private IEnumerable<StateDefinition<TState>> GetSubstates(StateDefinition<TState> state)
        {
            // Note: initial and final state cannot have substates.
            var result = state.StateKind == StateKind.Custom ?
                myStates.Values.Where(x => x.IsSubstate && myStateEqualityComparer.Equals(x.Parent, state.Value)) :
                Enumerable.Empty<StateDefinition<TState>>();
            return result;
        }

        private void FireImmediateTransitions()
        {
            var activeStates = myActiveStates.ToList();
            myActiveStates.Clear();

            // Go via active states.
            foreach (var activeState in activeStates)
            {
                var alreadyProcessed = new HashSet<TState>(myStateEqualityComparer);

                var stack = new Stack<Tree<StateRecord<TState, TTrigger>>>();
                stack.Push(activeState);

                while (stack.Count > 0)
                {
                    var thisState = stack.Pop();

                    var fromState = GetStateToContinue(thisState.Value.Definition);

                    if (!alreadyProcessed.Add(fromState.Value))
                    {
                        throw new InvalidOperationException("Immediate transitions cause the endless loop.");
                    }

                    var allTriggers = myGraph.GetEdgesGoingFrom(fromState.Value);
                    var immediateTriggers = allTriggers.Where(x => x.Value.Equals(myImmediateTransitRule));

                    // If immediate triggers exist then resolve them.
                    if (immediateTriggers.Any())
                    {
                        foreach (var edge in immediateTriggers)
                        {
                            if (myStates.TryGetValue(edge.To, out var toState))
                            {
                                var toStates = GetStatesToStart(toState);
                                foreach (var stateToContinue in toStates)
                                {
                                    var newRecord = new Tree<StateRecord<TState, TTrigger>>(new StateRecord<TState, TTrigger>(stateToContinue, edge.Value, default(TTrigger)));
                                    thisState.Add(newRecord);
                                    stack.Push(newRecord);
                                }
                            }
                        }

                        // If there are non-immediate triggers too.
                        var nonImmediateTriggers = allTriggers.Where(x => !x.Value.Equals(myImmediateTransitRule));
                        if (nonImmediateTriggers.Any())
                        {
                            myActiveStates.Add(thisState);
                        }
                    }
                    else
                    {
                        // Store the state without immediate triggers.
                        myActiveStates.Add(thisState);
                    }
                }
            }
        }
    }
}
