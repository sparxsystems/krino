using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using System.Diagnostics;
using System.Numerics;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Rules
{
    [DebuggerDisplay("{DebugView}")]
    public class WordAttributesRule : RuleBase<IWord>, IRule<IWord>
    {
        private ValueIsInRule myRule;

        public WordAttributesRule(BigInteger attribute)
        {
            myRule = ValueIsInRule.Is(attribute);
        }

        public BigInteger Attribute => myRule.Value;

        public override bool Evaluate(IWord value) => myRule.Evaluate(value.Attributes);

        public override bool Equals(IRule<IWord> other) => other is WordAttributesRule rule && myRule.Equals(rule.myRule);

        public override int GetHashCode() => myRule.GetHashCode();

        private string DebugView => GrammarAttributes.Instance.GetFullName(Attribute);
    }
}
