using Krino.Vertical.Utils.Rules;
using NUnit.Framework;

namespace Krino.Vertical.Utils_Tests.Rules
{
    [TestFixture]
    public class ExpressionRuleTest
    {
        [Test]
        public void Equals()
        {
            var rule1 = RuleMaker.Expression<string>(x => x.StartsWith("c"));
            var rule2 = RuleMaker.Expression<string>(x => x.StartsWith("c"));
            Assert.IsTrue(rule1.Equals(rule2));

            rule1 = RuleMaker.Expression<string>(x => x.StartsWith("c"));
            rule2 = RuleMaker.Expression<string>(x => x.StartsWith("d"));
            Assert.IsFalse(rule1.Equals(rule2));

            // Note: this is considered as a different expression.
            //       Maybe it can be improved in the future.
            rule1 = RuleMaker.Expression<string>(x => x.StartsWith("c"));
            rule2 = RuleMaker.Expression<string>(y => y.StartsWith("c"));
            Assert.IsFalse(rule1.Equals(rule2));
        }
    }
}
