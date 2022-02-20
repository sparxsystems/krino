using Krino.Vertical.Utils.Rules;
using System;

namespace Krino.Vertical.Utils.StateMachines
{
    public class TransitionRule<TState, TTrigger> : IEquatable<TransitionRule<TState, TTrigger>>
    {
        public IRule<StatePath<TState, TTrigger>> PathRule { get; set; } = RuleMaker.Anything<StatePath<TState, TTrigger>>();
        public IRule<TState> FromStateRule { get; set; } = RuleMaker.Anything<TState>();
        public IRule<TState> ToStateRule { get; set; } = RuleMaker.Anything<TState>();
        public IRule<TTrigger> TriggerRule { get; set; } = RuleMaker.Anything<TTrigger>();

        public virtual bool Evaluate(StatePath<TState, TTrigger> stateTrace, TState fromState, TState toState, TTrigger trigger)
        {
            var result = PathRule.Evaluate(stateTrace) &&
                         FromStateRule.Evaluate(fromState) &&
                         ToStateRule.Evaluate(toState) &&
                         TriggerRule.Evaluate(trigger);
            return result;
        }


        public bool Equals(TransitionRule<TState, TTrigger> other) =>
            PathRule.Equals(other.PathRule) &&
            FromStateRule.Equals(other.FromStateRule) &&
            ToStateRule.Equals(other.ToStateRule) &&
            TriggerRule.Equals(other.TriggerRule);

        public override bool Equals(object obj) => obj is TransitionRule<TState, TTrigger> otherRule && Equals(otherRule);

        public override int GetHashCode() => HashCode.Combine(PathRule, FromStateRule, ToStateRule, TriggerRule);
    }
}
