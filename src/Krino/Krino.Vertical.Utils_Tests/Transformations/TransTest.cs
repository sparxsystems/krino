using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.Transformations;
using NUnit.Framework;

namespace Krino.Vertical.Utils_Tests.Transformations
{
    [TestFixture]
    public class TransTest
    {
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
        public void Then()
        {
            ITransformation<int> t = Trans.ReplaceWith(10).ContinueWith(Trans.ReplaceWith(5));
            Assert.AreEqual(5, t.Transform(50));
        }

        [Test]
        public void DropFirst()
        {
            var t = Trans.DropFirst(0);
            Assert.AreEqual("", t.Transform("abc"));
            Assert.AreEqual("", t.Transform("ab"));
            Assert.AreEqual("", t.Transform("abcd"));
            Assert.AreEqual("", t.Transform(""));
            Assert.IsNull(t.Transform(null));

            t = Trans.DropFirst(0, 3);
            Assert.AreEqual("", t.Transform("abc"));
            Assert.AreEqual("", t.Transform("ab"));
            Assert.AreEqual("d", t.Transform("abcd"));
            Assert.AreEqual("", t.Transform(""));
            Assert.IsNull(t.Transform(null));

            t = Trans.DropFirst(1, 2);
            Assert.AreEqual("a", t.Transform("abc"));
            Assert.AreEqual("a", t.Transform("ab"));
            Assert.AreEqual("ad", t.Transform("abcd"));
            Assert.AreEqual("", t.Transform(""));
            Assert.IsNull(t.Transform(null));
        }

        [Test]
        public void DropLast()
        {
            var t = Trans.DropFromEnd(0);
            Assert.AreEqual("", t.Transform("abc"));
            Assert.AreEqual("", t.Transform("ab"));
            Assert.AreEqual("", t.Transform("abcd"));
            Assert.AreEqual("", t.Transform(""));
            Assert.IsNull(t.Transform(null));

            t = Trans.DropFromEnd(0, 3);
            Assert.AreEqual("", t.Transform("abc"));
            Assert.AreEqual("", t.Transform("ab"));
            Assert.AreEqual("a", t.Transform("abcd"));
            Assert.AreEqual("", t.Transform(""));
            Assert.IsNull(t.Transform(null));

            t = Trans.DropFromEnd(1, 2);
            Assert.AreEqual("c", t.Transform("abc"));
            Assert.AreEqual("b", t.Transform("ab"));
            Assert.AreEqual("ad", t.Transform("abcd"));
            Assert.AreEqual("", t.Transform(""));
            Assert.IsNull(t.Transform(null));

            t = Trans.DropFromEnd(1, 0);
            Assert.AreEqual("abc", t.Transform("abc"));

            t = Trans.DropFromEnd(5, 2);
            Assert.AreEqual("abc", t.Transform("abc"));
        }

        [Test]
        public void Replace()
        {
            var t = Trans.Replace("a", "b");
            Assert.AreEqual("bbc", t.Transform("abc"));
            Assert.AreEqual("xy", t.Transform("xy"));
            Assert.AreEqual("", t.Transform(""));
            Assert.IsNull(t.Transform(null));
        }
    }
}
