using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Rules;
using System.Diagnostics;
using System.Linq;
using System.Numerics;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Rules
{
    [DebuggerDisplay("{DebugView}")]
    public class WordContainsAttributeExactlyRule : RuleBase<IWord>, IRule<IWord>
    {
        public WordContainsAttributeExactlyRule(BigInteger attribute)
        {
            Attribute = attribute;
        }

        public BigInteger Attribute { get; private set; }

        public override bool Evaluate(IWord word)
        {
            var enums = GrammarAttributes.Instance.FindEnums(word.Attributes);
            var result = enums != null ?
                enums.Any(x => x == word.Attributes) :
                false;

            return result;
        }

        public override bool Equals(IRule<IWord> other) => other is WordContainsAttributeExactlyRule rule && Attribute == rule.Attribute;

        public override int GetHashCode() => Attribute.GetHashCode();

        private string DebugView => GrammarAttributes.Instance.GetFullName(Attribute);
    }
}
