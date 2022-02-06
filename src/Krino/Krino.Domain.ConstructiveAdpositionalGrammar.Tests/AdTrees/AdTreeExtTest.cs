using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeExtTest
    {
        [Test]
        public void GetSequenceToRoot()
        {
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("read", 0)),
                    Left = new AdTree(new Morpheme("I", 0))
                },
                Left = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("book", 0)),
                    Left = new AdTree(new Morpheme("the", 0))
                }
            };

            List<IAdTree> sequence = adTree.Right.Left.GetSequenceToRoot().ToList();
            Assert.AreEqual(3, sequence.Count);
            Assert.IsTrue(adTree.Right.Left == sequence[0]);
            Assert.IsTrue(adTree.Right == sequence[1]);
            Assert.IsTrue(adTree == sequence[2]);
        }

        [Test]
        public void GetPath()
        {
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("read", 0)),
                    Left = new AdTree(new Morpheme("I", 0))
                },
                Left = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("book", 0)),
                    Left = new AdTree(new Morpheme("the", 0))
                }
            };

            AdTreePosition[] path = adTree.Right.Left.GetPath();
            Assert.AreEqual(2, path.Length);
            Assert.AreEqual(AdTreePosition.ChildOnRight, path[0]);
            Assert.AreEqual(AdTreePosition.ChildOnLeft, path[1]);

            // Root
            path = adTree.GetPath();
            Assert.AreEqual(0, path.Length);
        }

        [Test]
        public void TryGetAdTree()
        {
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("read", 0)),
                    Left = new AdTree(new Morpheme("I", 0))
                },
                Left = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("book", 0)),
                    Left = new AdTree(new Morpheme("the", 0))
                }
            };

            adTree.TryGetAdTree(new AdTreePosition[] { AdTreePosition.ChildOnRight, AdTreePosition.ChildOnLeft }, out IAdTree result);
            Assert.AreEqual("I", result.Morpheme.Value);

            // Root
            adTree.TryGetAdTree(new AdTreePosition[] { }, out IAdTree root);
            Assert.IsTrue(adTree == root);
        }

        [Test]
        public void GetFirstAdPositionOnLeft()
        {
            AdTree adTree = new AdTree(new Morpheme("", 0))
            {
                Right = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("read", 0)),
                    Left = new AdTree(new Morpheme("I", 0))
                },
                Left = new AdTree(new Morpheme("", 0))
                {
                    Right = new AdTree(new Morpheme("book", 0)),
                    Left = new AdTree(new Morpheme("the", 0))
                }
            };

            IAdTree result = adTree.Left.Right.GetFirstAdPositionOnLeft();
            Assert.IsTrue(adTree.Left == result);

            // It is already on left.
            result = adTree.Right.Left.GetFirstAdPositionOnLeft();
            Assert.IsTrue(adTree.Right.Left == result);

            // Root.
            result = adTree.GetFirstAdPositionOnLeft();
            Assert.IsNull(result);
        }
       
    }
}
