using Krino.Vertical.Utils.Rules;
using System.Numerics;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Rules
{
    public static class WordRules
    {
        public static WordStringIsRule WordIs(string word, bool caseSensitive = false) => new WordStringIsRule(word, caseSensitive);
        public static WordContainsAttributeRule WordContainsAttribute(BigInteger attribute) => new WordContainsAttributeRule(attribute);
        public static WordContainsAttributeExactlyRule WordContainsAttributeExactly(BigInteger attribute) => new WordContainsAttributeExactlyRule(attribute);

        public static RuleBase<IWord> WordIsOneOf(params string[] auxiliaryWords)
        {
            RuleBase<IWord> result = null;

            for (int i = 0; i < auxiliaryWords.Length; ++i)
            {
                var wordStr = auxiliaryWords[i];

                if (i == 0)
                {
                    result = WordIs(wordStr, false);
                }
                else
                {
                    result |= WordIs(wordStr, true);
                }
            }

            return result;
        }

        public static WordEndsWithRule WordEndsWith(string suffix) => new WordEndsWithRule(suffix);

        public static RuleBase<IWord> WordEndsWithOneOf(params string[] suffixes)
        {
            RuleBase<IWord> result = null;

            for (int i = 0; i < suffixes.Length; ++i)
            {
                var suffix = suffixes[i];

                if (i == 0)
                {
                    result = WordEndsWith(suffix);
                }
                else
                {
                    result |= WordEndsWith(suffix);
                }
            }

            return result;
        }
    }
}
