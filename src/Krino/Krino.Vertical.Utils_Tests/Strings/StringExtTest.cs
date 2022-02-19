using Krino.Vertical.Utils.Strings;
using NUnit.Framework;

namespace Krino.Vertical.Utils_Tests.Strings
{
    [TestFixture]
    public class StringExtTest
    {
        [Test]
        public void Distance()
        {
            // 'o' must be replaced by 'y', 'u' must be added, 'i' must be added. = 3 changes. 
            Assert.AreEqual(3, "honda".Distance("hyundai"));

            // 1 letter must be added.
            Assert.AreEqual(1, "writ".Distance("write"));
        }

        [Test]
        public void JoinIgnoreEmpty()
        {
            Assert.AreEqual("a,b", StringExt.JoinIgnoreEmpty(",", "", null, "a", "", null, "b", "", null));
        }
    }
}
