namespace Krino.Vertical.Utils.Rules
{
    public class AndRule<T> : IRule<T>
    {
        private IRule<T> mySubRule1;
        private IRule<T> mySubRule2;

        public AndRule(IRule<T> subRule1, IRule<T> subRule2)
        {
            mySubRule1 = subRule1;
            mySubRule2 = subRule2;
        }

        public bool Evaluate(T value) => mySubRule1.Evaluate(value) && mySubRule2.Evaluate(value);
    }
}
