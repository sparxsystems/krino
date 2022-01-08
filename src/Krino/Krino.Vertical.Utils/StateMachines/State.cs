﻿namespace Krino.Vertical.Utils.StateMachines
{
    internal class State<TState>
    {
        private TState myParent;
        private bool myIsParentSet;

        public State(TState value, StateKind stateKind)
        {
            Value = value;
            StateKind = stateKind;
        }

        public TState Value { get; private set; }

        public StateKind StateKind { get; private set; }

        public TState Parent
        {
            get => myParent;
            set
            {
                myIsParentSet = true;
                myParent = value;
            }
        }

        public bool IsSubstate => myIsParentSet;
    }
}
