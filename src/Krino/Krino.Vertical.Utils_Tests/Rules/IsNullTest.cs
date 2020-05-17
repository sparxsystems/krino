using Krino.Vertical.Utils.Rules;
using NUnit.Framework;

namespace Krino.Vertical.Utils_Tests.Rules
{
    [TestFixture]
    public class IsNullTest
    {
        [Test]
        public void Evaluate()
        {
            IsNullRule<string> rule = new IsNullRule<string>();
            Assert.IsTrue(rule.Evaluate(null));
            Assert.IsFalse(rule.Evaluate("hello"));
        }
    }
}
