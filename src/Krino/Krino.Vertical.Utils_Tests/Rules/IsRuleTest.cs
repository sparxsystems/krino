using Krino.Vertical.Utils.Rules;
using NUnit.Framework;

namespace Krino.Vertical.Utils_Tests.Rules
{
    [TestFixture]
    public class IsRuleTest
    {
        [Test]
        public void ImplicitOperator()
        {
            IsRule<string> rule = "hello";
            Assert.IsTrue(rule.Evaluate("hello"));
            Assert.IsFalse(rule.Evaluate("hell"));
        }
    }
}
