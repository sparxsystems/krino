using Krino.Vertical.Utils.Strings;
using NUnit.Framework;

namespace Krino.Vertical.Utils_Tests.Strings
{
    [TestFixture]
    public class StringExtTest
    {
        [Test]
        public void StartsWithSameCount()
        {
            Assert.AreEqual(1, "a".StartsWithSameCount("a"));
            Assert.AreEqual(4, "abcd".StartsWithSameCount("abcd"));
            Assert.AreEqual(3, "abcd".StartsWithSameCount("abcx"));

            Assert.AreEqual(0, "abcd".StartsWithSameCount("zbcx"));
            Assert.AreEqual(0, "abcd".StartsWithSameCount(""));
            Assert.AreEqual(0, "abcd".StartsWithSameCount(null));
            Assert.AreEqual(0, "".StartsWithSameCount("ab"));
            Assert.AreEqual(0, "".StartsWithSameCount(""));
            Assert.AreEqual(0, "".StartsWithSameCount(null));
            Assert.AreEqual(0, ((string)null).StartsWithSameCount("ab"));
            Assert.AreEqual(0, ((string)null).StartsWithSameCount(""));
            Assert.AreEqual(0, ((string)null).StartsWithSameCount(null));
        }

        [Test]
        public void Distance()
        {
            // 'o' must be replaced by 'y', 'u' must be added, 'i' must be added. = 3 changes. 
            Assert.AreEqual(3, "honda".Distance("hyundai"));

            // 1 letter must be added.
            Assert.AreEqual(1, "writ".Distance("write"));

            Assert.AreEqual(2, "colli".Distance("collide"));
        }

        [Test]
        public void JoinIgnoreEmpty()
        {
            Assert.AreEqual("a,b", StringExt.JoinIgnoreEmpty(",", "", null, "a", "", null, "b", "", null));
        }
    }
}
