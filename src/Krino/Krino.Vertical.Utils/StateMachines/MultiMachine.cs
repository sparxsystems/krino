using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Graphs;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.StateMachines
{
    public class MultiMachine<TState, TTrigger>
    {
        private DirectedGraph<TState, TransitionRule<TState, TTrigger>> myGraph;
        private Dictionary<TState, StateDefinition<TState>> myStates = new Dictionary<TState, StateDefinition<TState>>();
        private ImmediateTriggerRule<TTrigger> myTriggerImmediatelyRule = new ImmediateTriggerRule<TTrigger>();

        private Tree<StateItem<TState, TTrigger>> myMachineTrack = new Tree<StateItem<TState, TTrigger>>(null);
        private List<Tree<StateItem<TState, TTrigger>>> myActiveStates = new List<Tree<StateItem<TState, TTrigger>>>();
        private List<Tree<StateItem<TState, TTrigger>>> myUnhandledStates = new List<Tree<StateItem<TState, TTrigger>>>();
        

        private IEqualityComparer<TState> myStateEqualityComparer;
        private IEqualityComparer<TTrigger> myTriggerEqualityComparer;


        public MultiMachine(IEqualityComparer<TState> stateComparer = null, IEqualityComparer<TTrigger> triggerComparer = null)
        {
            myStateEqualityComparer = stateComparer ?? EqualityComparer<TState>.Default;
            myTriggerEqualityComparer = triggerComparer ?? EqualityComparer<TTrigger>.Default;

            // Note: TransitionRule implements IEquatable, therefore the default comparer is ok to use for triggers.
            myGraph = new DirectedGraph<TState, TransitionRule<TState, TTrigger>>(stateComparer, null);

            DefaultTriggerRuleFactoryMethod = x => RuleMaker.Is(x, myTriggerEqualityComparer);
        }

        public Func<TTrigger, IRule<TTrigger>> DefaultTriggerRuleFactoryMethod { get; set; }


        public IEnumerable<TState> States => myGraph;

        public IEnumerable<StateTrace<TState, TTrigger>> GetActiveStates()
        {
            var result = myActiveStates.Select(x => new StateTrace<TState, TTrigger>(x.GetPathToRoot().Reverse().Select(y => y.Value)));
            return result;
        }

        public bool IsActive => myActiveStates.Any();


        public IEnumerable<StateTrace<TState, TTrigger>> GetUnhandledStates()
        {
            var result = myUnhandledStates.Select(x => new StateTrace<TState, TTrigger>(x.GetPathToRoot().Reverse().Select(y => y.Value)));
            return result;
        }

        public bool ContainsState(TState state) => myGraph.ContainsVertex(state);

        public bool TryGetStateDefinition(TState state, out StateDefinition<TState> result) => myStates.TryGetValue(state, out result);

        public void AddInitialState(TState state) => AddState(state, StateKind.Initial);

        public void AddState(TState state) => AddState(state, StateKind.Custom);

        public void AddFinalState(TState state) => AddState(state, StateKind.Final);


        public void AddInitialSubState(TState parent, TState subState) => AddSubState(parent, subState, StateKind.Initial);

        public void AddSubState(TState parent, TState subState) => AddSubState(parent, subState, StateKind.Custom);

        public void AddFinalSubState(TState parent, TState subState) => AddSubState(parent, subState, StateKind.Final);


        public void AddTransition(TState from, TState to) => AddTransition(from, to, myTriggerImmediatelyRule);

        public void AddTransition(TState from, TState to, TTrigger trigger) => AddTransition(from, to, DefaultTriggerRuleFactoryMethod(trigger));

        public void AddTransition(TState from, TState to, IRule<TTrigger> triggerRule) => AddTransition(from, to, new TransitionRule<TState, TTrigger>(null, null, null, triggerRule));

        public void AddTransition(TState from, TState to, TransitionRule<TState, TTrigger> transitionRule) => myGraph.AddEdge(from, to, transitionRule);


        public void Fire(TTrigger trigger)
        {
            var activeStates = myActiveStates.ToList();

            myActiveStates.Clear();
            myUnhandledStates.Clear();

            foreach (var activeState in activeStates)
            {
                var stateTrace = new StateTrace<TState, TTrigger>(activeState.GetPathToRoot().Reverse().Select(x => x.Value));

                var fromState = GetStateToContinueFrom(activeState.Value.Definition);

                var edgesFromState = myGraph.GetEdgesGoingFrom(fromState.Value);
                var applicableEdges = edgesFromState.Where(x => !x.Value.TriggerRule.Equals(myTriggerImmediatelyRule) && x.Value.Evaluate(stateTrace, x.From, x.To, trigger));
                if (applicableEdges.Any())
                {
                    foreach (var edge in applicableEdges)
                    {
                        if (myStates.TryGetValue(edge.To, out var toState))
                        {
                            var toStates = GetStatesToStart(toState);
                            foreach (var stateToContinue in toStates)
                            {
                                var newRecord = new Tree<StateItem<TState, TTrigger>>(new StateItem<TState, TTrigger>(stateToContinue, edge.Value, trigger));
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
            var newRecords = allTopLevelInitialStates.Select(x => new Tree<StateItem<TState, TTrigger>>(new StateItem<TState, TTrigger>(x, null, default(TTrigger))));

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

        private StateDefinition<TState> GetStateToContinueFrom(StateDefinition<TState> state)
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
                var stateTrace = new StateTrace<TState, TTrigger>(activeState.GetPathToRoot().Reverse().Select(x => x.Value));

                var alreadyProcessed = new HashSet<TState>(myStateEqualityComparer);

                var stack = new Stack<Tree<StateItem<TState, TTrigger>>>();
                stack.Push(activeState);

                while (stack.Count > 0)
                {
                    var thisState = stack.Pop();

                    var fromState = GetStateToContinueFrom(thisState.Value.Definition);

                    if (!alreadyProcessed.Add(fromState.Value))
                    {
                        throw new InvalidOperationException("Immediate transitions cause the endless loop.");
                    }

                    var edgesFromState = myGraph.GetEdgesGoingFrom(fromState.Value);
                    var immediateTriggers = edgesFromState.Where(x => x.Value.TriggerRule.Equals(myTriggerImmediatelyRule) && x.Value.Evaluate(stateTrace, x.From, x.To, default(TTrigger)));

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
                                    var newRecord = new Tree<StateItem<TState, TTrigger>>(new StateItem<TState, TTrigger>(stateToContinue, edge.Value, default(TTrigger)));
                                    thisState.Add(newRecord);
                                    stack.Push(newRecord);
                                }
                            }
                        }

                        // If there are non-immediate triggers too.
                        var nonImmediateTriggers = edgesFromState.Where(x => !x.Value.TriggerRule.Equals(myTriggerImmediatelyRule));
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
