using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Constructions.Rules
{
    [TestFixture]
    public class MaskRuleTest
    {
        [Test]
        public void Something()
        {
            Assert.IsTrue(MaskRule.Something.Evaluate(1));
            Assert.IsFalse(MaskRule.Something.Evaluate(0));
        }
    }
}
