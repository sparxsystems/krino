using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing.Rules
{
    public class PreviousWordAttributesRule : RuleBase<StatePath<LinguisticState, IWord>>, IRule<StatePath<LinguisticState, IWord>>
    {
        private BigInteger myAttribute;

        public PreviousWordAttributesRule(BigInteger attribute)
        {
            myAttribute = attribute;
        }


        public override bool Evaluate(StatePath<LinguisticState, IWord> value)
        {
            var result = value.Path.Reverse().Any(x => x.ByTrigger != null && EnumBase.IsIn(myAttribute, x.ByTrigger.Attributes));
            return result;
        }

        public override bool Equals(IRule<StatePath<LinguisticState, IWord>> other) => other is PreviousWordAttributesRule rule && myAttribute == rule.myAttribute;
    }
}
