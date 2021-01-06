using Krino.Vertical.Utils.Rules;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules.Strings
{
    /// <summary>
    /// Evaluates true if the string ends with the specified postfix string.
    /// </summary>
    public class SuffixRule : IRule<string>
    {
        private string mySuffix;

        public SuffixRule(string suffix)
        {
            mySuffix = suffix;
        }

        public bool Equals(IRule<string> other) => other is SuffixRule postfixRule && mySuffix == postfixRule.mySuffix;

        public bool Evaluate(string value) => value != null && value.EndsWith(mySuffix);
    }
}
