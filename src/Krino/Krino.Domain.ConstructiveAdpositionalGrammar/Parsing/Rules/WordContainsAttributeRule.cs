using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing.Rules
{
    [DebuggerDisplay("{DebugView}")]
    internal class WordContainsAttributeRule : IRule<IWord>
    {
        private ValueIsInRule myRule;

        public WordContainsAttributeRule(BigInteger attribute)
        {
            myRule = ValueIsInRule.Is(attribute);
        }

        public BigInteger Attribute => myRule.Value;

        public bool Equals(IRule<IWord> other) => other is WordContainsAttributeRule rule && myRule.Equals(rule.myRule);

        public bool Evaluate(IWord value) => myRule.Evaluate(value.Attributes);

        private string DebugView => Attribute.ToString();
    }
}
