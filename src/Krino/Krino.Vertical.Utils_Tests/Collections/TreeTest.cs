using Krino.Vertical.Utils.Collections;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;


namespace Krino.Vertical.Utils_Tests.Collections
{
    [TestFixture]
    public class TreeTest
    {
        private ITree<int> myTree = new Tree<int>(1)
            {
                new Tree<int>(11){ new Tree<int>(111), new Tree<int>(112) },
                new Tree<int>(12){ new Tree<int>(121), new Tree<int>(122) },
                new Tree<int>(13){ new Tree<int>(131), new Tree<int>(132) },
            };



        [Test]
        public void Count()
        {
            Assert.AreEqual(10, myTree.Count);
        }

        [Test]
        public void Generation()
        {
            // The root.
            Assert.AreEqual(0, myTree.Generation);

            // Child.
            ITree<int> aChild = myTree.FirstOrDefault(x => x.Value == 122);
            Assert.AreEqual(2, aChild.Generation);
        }

        [Test]
        public void ToList()
        {
            List<ITree<int>> aList = myTree.ToList();

            Assert.AreEqual(10, aList.Count);

            Assert.AreEqual(1, aList[0].Value);
            Assert.AreEqual(11, aList[1].Value);
            Assert.AreEqual(111, aList[2].Value);
            Assert.AreEqual(112, aList[3].Value);
            Assert.AreEqual(12, aList[4].Value);
            Assert.AreEqual(121, aList[5].Value);
            Assert.AreEqual(122, aList[6].Value);
            Assert.AreEqual(13, aList[7].Value);
            Assert.AreEqual(131, aList[8].Value);
            Assert.AreEqual(132, aList[9].Value);
        }

        [Test]
        public void Root()
        {
            ITree<int> aTreeItem = myTree.First(x => x.Value == 122);
            ITree<int> aRoot = aTreeItem.Root;

            Assert.AreEqual(1, aRoot.Value);
            Assert.AreEqual(myTree, aRoot);
        }

        [Test]
        public void Parent_Add()
        {
            // Note: Tree is going to be modified so in order not affect other tests
            // use local instance of the tree.
            Tree<int> aTree = new Tree<int>(1,
                new List<ITree<int>> {
                    new Tree<int>(11, new List<ITree<int>> { new Tree<int>(111), new Tree<int>(112) }),
                    new Tree<int>(12, new List<ITree<int>> { new Tree<int>(121), new Tree<int>(122) }),
                    new Tree<int>(13, new List<ITree<int>> { new Tree<int>(131), new Tree<int>(132) })
                });

            // Test if tree-items created via the constructor have the correct parent.
            ITree<int> aTreeItem = aTree.First(x => x.Value == 122);
            Assert.AreEqual(12, aTreeItem.Parent.Value);

            // Test if the item added explicitely has the correct parent.
            ITree<int> aNewItem = new Tree<int>(888);
            aTreeItem.Add(aNewItem);
            Assert.AreEqual(aTreeItem.Value, aNewItem.Parent.Value);
            Assert.IsTrue(aTreeItem.Children.Contains(aNewItem));
        }

        [Test]
        public void Parent_Set()
        {
            // Note: Tree is going to be modified so in order not affect other tests
            // use local instance of the tree.
            Tree<int> aTree = new Tree<int>(1,
                new List<ITree<int>> {
                    new Tree<int>(11, new List<ITree<int>> { new Tree<int>(111), new Tree<int>(112) }),
                    new Tree<int>(12, new List<ITree<int>> { new Tree<int>(121), new Tree<int>(122) }),
                    new Tree<int>(13, new List<ITree<int>> { new Tree<int>(131), new Tree<int>(132) })
                });

            // Test if tree-items created via the constructor have the correct parent.
            ITree<int> aTreeItem = aTree.First(x => x.Value == 122);
            Assert.AreEqual(12, aTreeItem.Parent.Value);

            // Test if the item added explicitely has the correct parent.
            ITree<int> aNewItem = new Tree<int>(888);
            aNewItem.Parent = aTreeItem;

            Assert.AreEqual(aTreeItem.Value, aNewItem.Parent.Value);
            Assert.IsTrue(aTreeItem.Children.Contains(aNewItem));
        }

        [Test]
        public void Parent_Set_Null()
        {
            // Note: Tree is going to be modified so in order not affect other tests
            // use local instance of the tree.
            Tree<int> aTree = new Tree<int>(1,
                new List<ITree<int>> {
                    new Tree<int>(11, new List<ITree<int>> { new Tree<int>(111), new Tree<int>(112) }),
                    new Tree<int>(12, new List<ITree<int>> { new Tree<int>(121), new Tree<int>(122) }),
                    new Tree<int>(13, new List<ITree<int>> { new Tree<int>(131), new Tree<int>(132) })
                });

            // Test if tree-items created via the constructor have the correct parent.
            ITree<int> aTreeItem = aTree.First(x => x.Value == 122);
            ITree<int> aParent = aTreeItem.Parent;
            Assert.AreEqual(12, aTreeItem.Parent.Value);

            aTreeItem.Parent = null;

            Assert.IsFalse(aParent.Children.Contains(aTreeItem));
        }

        [Test]
        public void Parents()
        {
            ITree<int> aTreeItem = myTree.First(x => x.Value == 122);
            IEnumerable<ITree<int>> aParentSequence = aTreeItem.Parents;

            Assert.AreEqual(2, aParentSequence.Count());

            Assert.AreEqual(12, aParentSequence.ElementAt(0).Value);
            Assert.AreEqual(1, aParentSequence.ElementAt(1).Value);
        }

        [Test]
        public void Siblings()
        {
            ITree<int> aTreeItem = myTree.First(x => x.Value == 12);
            IEnumerable<ITree<int>> aSiblings = aTreeItem.Siblings;

            Assert.AreEqual(2, aSiblings.Count());

            Assert.AreEqual(11, aSiblings.ElementAt(0).Value);
            Assert.AreEqual(13, aSiblings.ElementAt(1).Value);
        }

        [Test]
        public void Siblings_ParentNull()
        {
            IEnumerable<ITree<int>> aSiblings = myTree.Siblings;
            Assert.AreEqual(0, aSiblings.Count());
        }

        [Test]
        public void Leaves()
        {
            HashSet<int> aResult = myTree.Leaves.Select(x => x.Value).ToHashSet();
            HashSet<int> aExpected = new HashSet<int> { 111, 112, 121, 122, 131, 132 };
            Assert.True(aExpected.SetEquals(aResult));
        }

        [Test]
        public void Children()
        {
            IEnumerable<ITree<int>> aChildren = myTree.Children;

            Assert.AreEqual(3, aChildren.Count());

            Assert.AreEqual(11, aChildren.ElementAt(0).Value);
            Assert.AreEqual(12, aChildren.ElementAt(1).Value);
            Assert.AreEqual(13, aChildren.ElementAt(2).Value);
        }

        [Test]
        public void Add()
        {
            // Tree is going to be modified so in order not affect other tests
            // use local instance of the tree.
            ITree<int> aTree = new Tree<int>(1,
                new List<ITree<int>> {
                    new Tree<int>(11, new List<ITree<int>> { new Tree<int>(111), new Tree<int>(112) }),
                    new Tree<int>(12, new List<ITree<int>> { new Tree<int>(121), new Tree<int>(122) }),
                    new Tree<int>(13, new List<ITree<int>> { new Tree<int>(131), new Tree<int>(132) })
                });

            ITree<int> aTreeItem = aTree.First(x => x.Value == 122);

            // Add the tree item directly under the root.
            aTree.Add(aTreeItem);

            Assert.AreEqual(10, aTree.Count);
            Assert.AreEqual(4, aTree.Children.Count());
            Assert.AreEqual(aTree.Value, aTreeItem.Parent.Value);

            Assert.AreEqual(1, aTree.ElementAt(0).Value);
            Assert.AreEqual(11, aTree.ElementAt(1).Value);
            Assert.AreEqual(111, aTree.ElementAt(2).Value);
            Assert.AreEqual(112, aTree.ElementAt(3).Value);
            Assert.AreEqual(12, aTree.ElementAt(4).Value);
            Assert.AreEqual(121, aTree.ElementAt(5).Value);

            Assert.AreEqual(13, aTree.ElementAt(6).Value);
            Assert.AreEqual(131, aTree.ElementAt(7).Value);
            Assert.AreEqual(132, aTree.ElementAt(8).Value);

            Assert.AreEqual(122, aTree.ElementAt(9).Value);
        }

    }
}
