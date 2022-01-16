using Krino.Vertical.Utils.Rules;
using System;

namespace Krino.Vertical.Utils.StateMachines
{
    public class TransitionRule<TState, TTrigger> : IEquatable<TransitionRule<TState, TTrigger>>
    {
        public TransitionRule(IRule<StateTrace<TState, TTrigger>> traceRule, IRule<TState> fromStateRule, IRule<TState> toStateRule, IRule<TTrigger> triggerRule)
        {
            TraceRule = traceRule ?? RuleMaker.Anything<StateTrace<TState, TTrigger>>();
            FromStateRule = fromStateRule ?? RuleMaker.Anything<TState>();
            ToStateRule = toStateRule ?? RuleMaker.Anything<TState>();
            TriggerRule = triggerRule ?? RuleMaker.Anything<TTrigger>();
        }

        public IRule<StateTrace<TState, TTrigger>> TraceRule { get; }
        public IRule<TState> FromStateRule { get; }
        public IRule<TState> ToStateRule { get; }
        public IRule<TTrigger> TriggerRule { get; }

        public bool Evaluate(StateTrace<TState, TTrigger> stateTrace, TState fromState, TState toState, TTrigger trigger)
        {
            var result = TraceRule.Evaluate(stateTrace) &&
                         FromStateRule.Evaluate(fromState) &&
                         ToStateRule.Evaluate(toState) &&
                         TriggerRule.Evaluate(trigger);
            return result;
        }


        public bool Equals(TransitionRule<TState, TTrigger> other) =>
            TraceRule.Equals(other.TraceRule) &&
            FromStateRule.Equals(other.FromStateRule) &&
            ToStateRule.Equals(other.ToStateRule) &&
            TriggerRule.Equals(other.TriggerRule);

        public override bool Equals(object obj) => obj is TransitionRule<TState, TTrigger> otherRule && Equals(otherRule);

        public override int GetHashCode() => HashCode.Combine(TraceRule, FromStateRule, ToStateRule, TriggerRule);
    }
}
