using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing.Rules
{
    [DebuggerDisplay("{DebugView}")]
    internal class WordContainsAttributeRule : RuleBase<IWord>, IRule<IWord>
    {
        private ValueIsInRule myRule;

        public WordContainsAttributeRule(BigInteger attribute)
        {
            myRule = ValueIsInRule.Is(attribute);
        }

        public BigInteger Attribute => myRule.Value;

        public override bool Evaluate(IWord value) => myRule.Evaluate(value.Attributes);

        public override bool Equals(IRule<IWord> other) => other is WordContainsAttributeRule rule && myRule.Equals(rule.myRule);

        private string DebugView => GrammarAttributes.Instance.GetFullName(Attribute);
    }
}
