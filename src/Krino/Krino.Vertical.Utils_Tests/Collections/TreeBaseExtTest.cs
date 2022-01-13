using Krino.Vertical.Utils.Collections;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils_Tests.Collections
{
    [TestFixture]
    public class TreeBaseExtTest
    {
        [Test]
        public void GetPathToRoot()
        {
            Tree<int> tree = new Tree<int>(1)
            {
                new Tree<int>(11){ new Tree<int>(111), new Tree<int>(112) },
                new Tree<int>(12){ new Tree<int>(121), new Tree<int>(122) },
                new Tree<int>(13){ new Tree<int>(131), new Tree<int>(132) },
            };

            var n122 = tree.First(x => x.Value == 122);
            
            var result = n122.GetPathToRoot().ToList();
            Assert.AreEqual(3, result.Count);
            Assert.AreEqual(122, result[0].Value);
            Assert.AreEqual(12, result[1].Value);
            Assert.AreEqual(1, result[2].Value);
        }

        [Test]
        public void MakeRoot()
        {
            Tree<int> tree = new Tree<int>(1)
            {
                new Tree<int>(11){ new Tree<int>(111), new Tree<int>(112) },
                new Tree<int>(12){ new Tree<int>(121), new Tree<int>(122) },
                new Tree<int>(13){ new Tree<int>(131), new Tree<int>(132) },
            };

            ITree<int> n122 = tree.First(x => x.Value == 122);

            // node 122 becomes the new root.
            n122.MakeRoot();
            Assert.IsNull(n122.Parent);

            List<int> pathToRoot = tree.Parents.Select(x => x.Value).ToList();
            Assert.AreEqual(2, pathToRoot.Count);
            Assert.AreEqual(12, pathToRoot[0]);
            Assert.AreEqual(122, pathToRoot[1]);

            ITree<int> n132 = n122.First(x => x.Value == 132);
            pathToRoot = n132.Parents.Select(x => x.Value).ToList();
            Assert.AreEqual(4, pathToRoot.Count);
            Assert.AreEqual(13, pathToRoot[0]);
            Assert.AreEqual(1, pathToRoot[1]);
            Assert.AreEqual(12, pathToRoot[2]);
            Assert.AreEqual(122, pathToRoot[3]);

            ITree<int> n12 = n122.First(x => x.Value == 12);
            pathToRoot = n12.Parents.Select(x => x.Value).ToList();
            Assert.AreEqual(1, pathToRoot.Count);
            Assert.AreEqual(122, pathToRoot[0]);
        }
    }
}
