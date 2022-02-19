using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Collections.Generic;
using System.Numerics;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing.Rules
{
    public class PredicateContainsRule : RuleBase<StatePath<LinguisticState, IWord>>, IRule<StatePath<LinguisticState, IWord>>
    {
        private ValueIsInRule myRule;

        public PredicateContainsRule(BigInteger attribute)
        {
            myRule = ValueIsInRule.Is(attribute);
        }

        public BigInteger Attribute => myRule.Value;

        public override bool Evaluate(StatePath<LinguisticState, IWord> value)
        {
            // Get last sentence.
            throw null;
        }

        public override bool Equals(IRule<StatePath<LinguisticState, IWord>> other) => other is PredicateContainsRule rule && myRule.Equals(rule.myRule);


        private string DebugView => Attribute.ToString();
    }
}
