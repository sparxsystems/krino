using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing.Rules;
using Krino.Vertical.Utils.Rules;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public static class WordRule
    {
        public static IRule<IWord> WordStringIs(string word, bool caseSensitive = false) => new WordStringIsRule(word, caseSensitive);
        public static IRule<IWord> WordContainsAttribute(BigInteger attribute) => new WordContainsAttributeRule(attribute);
    }
}
