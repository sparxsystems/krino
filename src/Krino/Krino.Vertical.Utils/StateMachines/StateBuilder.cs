using Krino.Vertical.Utils.Graphs;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Vertical.Utils.StateMachines
{
    public class StateBuilder<TState, TTrigger>
    {
        private List<TTrigger> myTriggers = new List<TTrigger>();

        internal StateBuilder(TState value, StateKind stateKind)
        {
            Value = value;
            StateKind = stateKind;
        }

        public TState Value { get; private set; }

        public StateKind StateKind { get; private set; }

        public TState Parent { get; private set; }

        public StateBuilder<TState, TTrigger> SubsetOf(TState parent)
        {
            Parent = parent;
            return this;
        }

        public StateBuilder<TState, TTrigger> AddTrigger(TTrigger trigger)
        {
            myTriggers.Add(trigger);
            return this;
        }
    }
}
