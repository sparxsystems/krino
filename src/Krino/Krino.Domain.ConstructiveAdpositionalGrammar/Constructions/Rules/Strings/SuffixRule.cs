using Krino.Vertical.Utils.Rules;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules.Strings
{
    /// <summary>
    /// Evaluates true if the string ends with the specified postfix string.
    /// </summary>
    public class SuffixRule : IRule<string>
    {
        private string myPostfix;

        public SuffixRule(string postfix)
        {
            myPostfix = postfix;
        }

        public bool Equals(IRule<string> other) => other is SuffixRule postfixRule && myPostfix == postfixRule.myPostfix;

        public bool Evaluate(string value) => value.EndsWith(myPostfix);
    }
}
