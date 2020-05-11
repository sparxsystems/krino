using Krino.Vertical.Utils.Rules;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules.Strings
{
    /// <summary>
    /// Returns true if the word ends with a consonant character.
    /// </summary>
    public class EndsWithConsonantRule : IRule<string>
    {
        private static HashSet<char> myConsonants = new HashSet<char>()
        {
            'b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'x', 'z'
        };

        public bool Evaluate(string value)
        {
            bool result = false;

            string lowerValue = value.ToLowerInvariant();

            char lastChar = lowerValue[value.Length - 1];
            if (myConsonants.Contains(lastChar))
            {
                result = true;
            }
            // w is a special case - if a vowel does not stand in front of w then it is the consonant.
            else if (lastChar == 'w')
            {
                if (lowerValue.Length > 1)
                {
                    char beforeLastChar = lowerValue[value.Length - 2];
                    if (!myConsonants.Contains(beforeLastChar))
                    {
                        result = true;
                    }
                }
                else
                {
                    result = true;
                }
            }

            return result;
        }

        public bool Equals(IRule<string> other) => other is EndsWithConsonantRule;
    }
}
