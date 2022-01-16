using System.Diagnostics;

namespace Krino.Vertical.Utils.StateMachines
{
    [DebuggerDisplay("{DebugView}")]
    public class StateDefinition<TState>
    {
        private TState myParent;
        private bool myIsParentSet;

        public StateDefinition(TState value, StateKind stateKind)
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

        private string DebugView => $"{StateKind}: {Value}";
    }
}
