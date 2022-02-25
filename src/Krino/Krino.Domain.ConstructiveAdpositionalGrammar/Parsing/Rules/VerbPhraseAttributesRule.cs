﻿using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.StateMachines;
using System.Diagnostics;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing.Rules
{
    [DebuggerDisplay("{DebugView}")]
    public class VerbPhraseAttributesRule : RuleBase<StatePath<LinguisticState, IWord>>, IRule<StatePath<LinguisticState, IWord>>
    {
        private ValueIsInRule myRule;

        public VerbPhraseAttributesRule(BigInteger attribute)
        {
            myRule = ValueIsInRule.Is(attribute);
        }

        public BigInteger Attribute => myRule.Value;

        public override bool Evaluate(StatePath<LinguisticState, IWord> value)
        {
            bool result = false;

            var verbPhrase = value.Path.GetLastVerbPhrase();
            if (verbPhrase != null)
            {
                var attributes = verbPhrase.Items.Select(x => x.Attributes).AccumulateEnums();
                result = myRule.Evaluate(attributes);
            }

            return result;
        }

        public override bool Equals(IRule<StatePath<LinguisticState, IWord>> other) => other is VerbPhraseAttributesRule rule && myRule.Equals(rule.myRule);


        private string DebugView => GrammarAttributes.Instance.GetFullName(Attribute);
    }
}