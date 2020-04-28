namespace Krino.Vertical.Utils.Rules
{
    public class NotRule<T> : IRule<T>
    {
        private IRule<T> mySubRule;

        public NotRule(IRule<T> subRule)
        {
            mySubRule = subRule;
        }

        public bool Evaluate(T value) => !mySubRule.Evaluate(value);
    }
}
