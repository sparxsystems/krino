using Krino.Vertical.Utils.Rules;
using NUnit.Framework;

namespace Krino.Vertical.Utils_Tests.Rules
{
    [TestFixture]
    public class ContainsRuleTest
    {
        [Test]
        public void Equals()
        {
            var rule1 = RuleMaker.Contains(null, "a", "b", "c");
            var rule2 = RuleMaker.Contains(null, "a", "b", "c");
            Assert.IsTrue(rule1.Equals(rule2));

            rule1 = RuleMaker.Contains(null, "a", "b", "c");
            rule2 = RuleMaker.Contains(null, "a", "b" );
            Assert.IsFalse(rule1.Equals(rule2));

            rule1 = RuleMaker.Contains(null, "a", "b", "c");
            var rule3 = RuleMaker.Is("a");
            Assert.IsFalse(rule1.Equals(rule3));
        }
    }
}
