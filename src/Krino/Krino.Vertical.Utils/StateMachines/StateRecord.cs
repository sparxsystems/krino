using Krino.Vertical.Utils.Rules;

namespace Krino.Vertical.Utils.StateMachines
{
    public class StateRecord<TState, TTrigger>
    {
        public StateRecord(StateRepresentation<TState> stateRepresenation, IRule<TTrigger> byTransitionRule, TTrigger byTrigger)
        {
            StateRepresentation = stateRepresenation;
            ByTransitionRule = byTransitionRule;
            ByTrigger = byTrigger;
        }

        public StateRepresentation<TState> StateRepresentation { get; private set; }

        public TTrigger ByTrigger { get; private set; }

        public IRule<TTrigger> ByTransitionRule { get; private set; }

        public bool IsUnhandled { get; internal set; }

        public TTrigger UnhandledTrigger { get; internal set; }
    }
}
