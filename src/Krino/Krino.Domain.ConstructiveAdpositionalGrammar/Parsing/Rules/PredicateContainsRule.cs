﻿using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.StateMachines;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing.Rules
{
    [DebuggerDisplay("{DebugView}")]
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
            bool result = false;

            var lastSentence = value.Path.GetLastClause();
            if (lastSentence?.Predicate != null)
            {
                myRule.Evaluate(lastSentence.Predicate.Attributes);
            }

            return result;
        }

        public override bool Equals(IRule<StatePath<LinguisticState, IWord>> other) => other is PredicateContainsRule rule && myRule.Equals(rule.myRule);


        private string DebugView => GrammarAttributes.Instance.GetFullName(Attribute);
    }
}
