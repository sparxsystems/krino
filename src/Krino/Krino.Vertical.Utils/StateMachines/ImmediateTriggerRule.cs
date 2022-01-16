using Krino.Vertical.Utils.Rules;

namespace Krino.Vertical.Utils.StateMachines
{
    /// <summary>
    /// Ensures the transition is performed immediately without waiting for a trigger.
    /// </summary>
    /// <typeparam name="TTrigger"></typeparam>
    public class ImmediateTriggerRule<TTrigger> : RuleBase<TTrigger>, IRule<TTrigger>
    {
        public override bool Equals(IRule<TTrigger> other) => other is ImmediateTriggerRule<TTrigger>;

        public override bool Evaluate(TTrigger value) => true;
        
    }
}
