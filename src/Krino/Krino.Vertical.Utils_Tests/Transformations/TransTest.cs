using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.Transformations;
using NUnit.Framework;

namespace Krino.Vertical.Utils_Tests.Transformations
{
    [TestFixture]
    public class TransTest
    {
        [Test]
        public void ReplaceWith()
        {
            ITransformation<int> t = Trans.ReplaceWith(10);
            Assert.AreEqual(10, t.Transform(5));
        }

        [Test]
        public void If_Then()
        {
            ITransformation<int> t = Trans.If(RuleMaker.Is(10), Trans.ReplaceWith(9));
            Assert.AreEqual(9, t.Transform(10));
            Assert.AreEqual(5, t.Transform(5));
        }

        [Test]
        public void If_Then_Else()
        {
            ITransformation<int> t = Trans.If(RuleMaker.Is(10), Trans.ReplaceWith(9)).Else(Trans.ReplaceWith(3));
            Assert.AreEqual(9, t.Transform(10));
            Assert.AreEqual(3, t.Transform(5));
        }

        [Test]
        public void If_Then_ElseIf_Else()
        {
            ITransformation<int> t =
                Trans.If(RuleMaker.Is(10), Trans.ReplaceWith(1))
                     .Else(Trans.If(RuleMaker.Is(9), Trans.ReplaceWith(2))
                           .Else(Trans.ReplaceWith(3))
                           );

            Assert.AreEqual(1, t.Transform(10));
            Assert.AreEqual(2, t.Transform(9));
            Assert.AreEqual(3, t.Transform(100));
        }

        [Test]
        public void ContinueWith()
        {
            ITransformation<int> t = Trans.ReplaceWith(10).ContinueWith(Trans.ReplaceWith(5));
            Assert.AreEqual(5, t.Transform(50));
        }
    }
}
