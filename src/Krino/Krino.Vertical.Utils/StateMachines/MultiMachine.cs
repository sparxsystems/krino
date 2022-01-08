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
        private Dictionary<TState, State<TState>> myStates = new Dictionary<TState, State<TState>>();
        private TransitImmediatelyRule<TTrigger> myImmediateTransitRule = new TransitImmediatelyRule<TTrigger>();

        private List<TState> myActiveStates = new List<TState>();
        private List<TState> myUnhandledStates = new List<TState>();

        private IEqualityComparer<TState> myStateEqualityComparer;


        public MultiMachine(IEqualityComparer<TState> stateEqualityComparer = null)
        {
            myStateEqualityComparer = stateEqualityComparer ?? EqualityComparer<TState>.Default;

            myGraph = new DirectedGraph<TState, IRule<TTrigger>>(stateEqualityComparer, null);

            Reset();
        }


        public IEnumerable<TState> ActiveStates => myActiveStates;

        public void AddState(TState state, StateKind stateKind)
        {
            myGraph.AddVertex(state);

            var newState = new State<TState>(state, stateKind);
            myStates[state] = newState;
        }

        public void AddSubState(TState parent, TState subState, StateKind stateKind)
        {
            myGraph.AddVertex(subState);

            var newState = new State<TState>(subState, stateKind)
            {
                Parent = parent,
            };
            myStates[subState] = newState;
        }

        public void AddTransition(TState from, TState to) => AddTransition(from, to, myImmediateTransitRule);

        public void AddTransition(TState from, TState to, TTrigger trigger) => AddTransition(from, to, RuleMaker.Is(trigger));

        public void AddTransition(TState from, TState to, IRule<TTrigger> triggerRule) => myGraph.AddEdge(from, to, triggerRule);

        public void Fire(TTrigger trigger)
        {
            FireImmediateTransitions();

            var activeStates = myActiveStates.ToList();

            myActiveStates.Clear();
            myUnhandledStates.Clear();

            foreach (var activeState in activeStates)
            {
                if (myStates.TryGetValue(activeState, out var stateItem))
                {
                    TState fromState;

                    // If it is a final state of a substate then continue from the parent state.
                    if (stateItem.IsSubstate && stateItem.StateKind == StateKind.Final)
                    {
                        fromState = stateItem.Parent;
                    }
                    else
                    {
                        fromState = activeState;
                    }

                    var triggersFromState = myGraph.GetEdgesGoingFrom(fromState);
                    var applicableTriggers = triggersFromState.Where(x => x.Value.Evaluate(trigger));
                    if (applicableTriggers.Any())
                    {
                        foreach (var edge in applicableTriggers)
                        {
                            if (myStates.TryGetValue(edge.To, out var toState))
                            {
                                // If the target state has sub-states then find the initial state.
                                var initialSubState = myStates.Values.FirstOrDefault(x => x.IsSubstate && x.StateKind == StateKind.Initial && myStateEqualityComparer.Equals(edge.To, x.Parent));
                                if (initialSubState != null)
                                {
                                    myActiveStates.Add(initialSubState.Value);
                                }
                                else
                                {
                                    myActiveStates.Add(edge.To);
                                }
                            }
                        }
                    }
                    else
                    {
                        myUnhandledStates.Add(fromState);
                    }
                }
            }
        }

        public void Reset()
        {
            myActiveStates.Clear();
            var initialState = myStates.Values.FirstOrDefault(x => x.StateKind == StateKind.Initial && !x.IsSubstate);
            if (initialState != null)
            {
                myActiveStates.Add(initialState.Value);
            }
        }

        private void FireImmediateTransitions()
        {
            var activeStates = myActiveStates.ToList();
            myActiveStates.Clear();

            // Go via active states.
            foreach (var activeState in activeStates)
            {
                var stack = new Stack<TState>();
                stack.Push(activeState);

                while (stack.Count > 0)
                {
                    var thisState = stack.Pop();

                    if (myStates.TryGetValue(activeState, out var stateItem))
                    {
                        TState stateToEvaluate;

                        // If it is a final state of a substate then continue from the parent state.
                        if (stateItem.IsSubstate && stateItem.StateKind == StateKind.Final)
                        {
                            stateToEvaluate = stateItem.Parent;
                        }
                        else
                        {
                            stateToEvaluate = stateItem.Value;
                        }

                        var allTriggers = myGraph.GetEdgesGoingFrom(stateToEvaluate);
                        var immediateTriggers = allTriggers.Where(x => x.Value.Equals(myImmediateTransitRule));

                        // If immediate triggers exist then resolve them.
                        if (immediateTriggers.Any())
                        {
                            foreach (var edge in immediateTriggers)
                            {
                                if (myStates.TryGetValue(edge.To, out var toState))
                                {
                                    // If the target state has sub-states then find the initial state.
                                    var initialSubState = myStates.Values.FirstOrDefault(x => x.IsSubstate && x.StateKind == StateKind.Initial && myStateEqualityComparer.Equals(edge.To, x.Parent));
                                    if (initialSubState != null)
                                    {
                                        stack.Push(initialSubState.Value);
                                    }
                                    else
                                    {
                                        stack.Push(edge.To);
                                    }
                                }
                            }

                            // If there are non-immediate triggers too.
                            var nonImmediateTriggers = allTriggers.Where(x => !x.Value.Equals(myImmediateTransitRule));
                            if (nonImmediateTriggers.Any())
                            {
                                myActiveStates.Add(stateToEvaluate);
                            }
                        }
                        else
                        {
                            // Store the state without immediate triggers.
                            myActiveStates.Add(stateToEvaluate);
                        }
                    }
                }
            }
        }
    }
}
