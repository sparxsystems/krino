using Krino.Vertical.Utils.Rules;

namespace Krino.Vertical.Utils.StateMachines
{
    /// <summary>
    /// Represent the record how the state machine processed a state.
    /// </summary>
    /// <typeparam name="TState"></typeparam>
    /// <typeparam name="TTrigger"></typeparam>
    public class StateRecord<TState, TTrigger>
    {
        public StateRecord(StateDefinition<TState> stateRepresenation, IRule<TTrigger> byTransitionRule, TTrigger byTrigger)
        {
            Definition = stateRepresenation;
            ByTransitionRule = byTransitionRule;
            ByTrigger = byTrigger;
        }

        /// <summary>
        /// State as defined in the state machine.
        /// </summary>
        public StateDefinition<TState> Definition { get; private set; }

        /// <summary>
        /// The trigger which which caused this state.
        /// </summary>
        public TTrigger ByTrigger { get; private set; }

        /// <summary>
        /// The transition rule which caused this state.
        /// </summary>
        public IRule<TTrigger> ByTransitionRule { get; private set; }

        /// <summary>
        /// True if the incoming trigger could not be processed by this state.
        /// </summary>
        public bool IsUnhandled { get; internal set; }

        /// <summary>
        /// The trigger which could not be processed by this state.
        /// </summary>
        public TTrigger UnhandledTrigger { get; internal set; }
    }
}
