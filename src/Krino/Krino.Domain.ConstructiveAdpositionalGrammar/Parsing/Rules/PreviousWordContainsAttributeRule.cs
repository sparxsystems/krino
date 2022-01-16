using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing.Rules
{
    public class PreviousWordContainsAttributeRule : RuleBase<StateTrace<LinguisticState, IWord>>, IRule<StateTrace<LinguisticState, IWord>>
    {
        private BigInteger myAttribute;

        public PreviousWordContainsAttributeRule(BigInteger attribute)
        {
            myAttribute = attribute;
        }


        public override bool Evaluate(StateTrace<LinguisticState, IWord> value)
        {
            var result = value.Trace.Reverse().Any(x => x.ByTrigger != null && EnumBase.IsIn(myAttribute, x.ByTrigger.Attributes));
            return result;
        }

        public override bool Equals(IRule<StateTrace<LinguisticState, IWord>> other) => other is PreviousWordContainsAttributeRule rule && myAttribute == rule.myAttribute;
    }
}
