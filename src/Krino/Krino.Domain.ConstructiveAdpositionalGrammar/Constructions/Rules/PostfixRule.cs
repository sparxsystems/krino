using Krino.Vertical.Utils.Rules;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Evaluates true if the string ends with the specified postfix string.
    /// </summary>
    public class PostfixRule : IRule<string>
    {
        public static PostfixRule Is(string postfix) => new PostfixRule(postfix);


        private string myPostfix;

        public PostfixRule(string postfix)
        {
            myPostfix = postfix;
        }

        public bool Equals(IRule<string> other) => other is PostfixRule postfixRule && myPostfix == postfixRule.myPostfix;

        public bool Evaluate(string value) => value.EndsWith(myPostfix);
    }
}
