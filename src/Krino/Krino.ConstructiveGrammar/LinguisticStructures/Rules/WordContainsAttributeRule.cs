using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using System.Diagnostics;
using System.Numerics;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Rules
{
    [DebuggerDisplay("{DebugView}")]
    public class WordContainsAttributeRule : RuleBase<IWord>, IRule<IWord>
    {
        private ValueIsInAttributesRule myValueIsIn;

        public WordContainsAttributeRule(BigInteger value)
        {
            myValueIsIn = ValueIsInAttributesRule.Is(value);
        }

        public BigInteger Attribute => myValueIsIn.Value;

        public override bool Evaluate(IWord word) => myValueIsIn.Evaluate(word.Attributes);

        public override bool Equals(IRule<IWord> other) => other is WordContainsAttributeRule rule && myValueIsIn.Equals(rule.myValueIsIn);

        public override int GetHashCode() => myValueIsIn.GetHashCode();

        private string DebugView => GrammarAttributes.Instance.GetFullName(Attribute);
    }
}
