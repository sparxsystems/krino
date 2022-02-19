using System.Diagnostics;
using System.Reflection;

namespace Krino.Vertical.Utils.StateMachines
{
    /// <summary>
    /// Represent the record how the state machine processed a state.
    /// </summary>
    /// <typeparam name="TState"></typeparam>
    /// <typeparam name="TTrigger"></typeparam>
    [DebuggerDisplay("{DebugView}")]
    public class StateItem<TState, TTrigger>
    {
        public StateItem(StateDefinition<TState> stateRepresenation, TransitionRule<TState, TTrigger> byTransitionRule, TTrigger byTrigger)
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
        /// The trigger which caused this state.
        /// </summary>
        public TTrigger ByTrigger { get; private set; }

        /// <summary>
        /// The transition rule applied to get into this state.
        /// </summary>
        public TransitionRule<TState, TTrigger> ByTransitionRule { get; private set; }

        public bool IsImmediateTransition => ByTransitionRule?.TriggerRule is ImmediateTriggerRule<TTrigger>;

        /// <summary>
        /// True if the received trigger could not be processed by this state.
        /// </summary>
        public bool IsUnhandled { get; set; }

        /// <summary>
        /// The trigger which could not be handled by this state.
        /// </summary>

        public TTrigger UnhandledTrigger { get; set; }

        private string DebugView
        {
            get
            {
                var result = Definition.Value.GetType().GetProperty("DebugView", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance)?.GetValue(Definition.Value, null) as string;
                result ??= $"{Definition.Value}";
                return result;
            }
        }
    }
}
