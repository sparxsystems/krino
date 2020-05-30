﻿using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
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
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("the", 0), new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("the", 0), new Pattern())
                }
            };

            AttachPosition[] path = adTree.Right.Left.GetPath();
            Assert.AreEqual(2, path.Length);
            Assert.AreEqual(AttachPosition.ChildOnRight, path[0]);
            Assert.AreEqual(AttachPosition.ChildOnLeft, path[1]);

            // Root
            path = adTree.GetPath();
            Assert.AreEqual(0, path.Length);
        }

        [Test]
        public void TryGetAdTree()
        {
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("the", 0), new Pattern())
                }
            };

            adTree.TryGetAdTree(new AttachPosition[] { AttachPosition.ChildOnRight, AttachPosition.ChildOnLeft }, out IAdTree result);
            Assert.AreEqual("I", result.Morpheme.Morph);

            // Root
            adTree.TryGetAdTree(new AttachPosition[] { }, out IAdTree root);
            Assert.IsTrue(adTree == root);
        }

        [Test]
        public void CanAttachToRight()
        {
            IAdTree adTree = new AdTree(new Morpheme("", 0),
                new Pattern() { RightRule = MorphemeRule.O_Lexeme, }
            );
            IAdTree adTreeElement = new AdTree(new Morpheme("", 0),
                new Pattern() { MorphemeRule = MorphemeRule.O_Lexeme });
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement));


            adTree = new AdTree(new Morpheme("", 0),
                new Pattern() { RightRule = MorphemeRule.O_Lexeme, }
            );
            adTreeElement = new AdTree(new Morpheme("", 0),
                new Pattern() { RightRule = MorphemeRule.A_Lexeme });
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement));
        }

        [Test]
        public void CanAttachToRight_Inheritance()
        {
            IAdTree adTree = new AdTree(new Morpheme("", 0),
                new Pattern() { RightRule = MorphemeRule.O_Lexeme, }
            );
            IAdTree adTreeElement = new AdTree(new Morpheme("", 0),
                new Pattern() { MorphemeRule = MorphemeRule.Epsilon, RightRule = MorphemeRule.O_Lexeme });
            Assert.IsTrue(adTree.CanAttachToRight(adTreeElement));


            // Inheritance is not allowed.
            adTree = new AdTree(new Morpheme("", 0),
                new Pattern() { RightRule = MorphemeRule.O_Lexeme.SetInheritance(InheritanceRuleMaker.Nothing) }
            );
            adTreeElement = new AdTree(new Morpheme("", 0),
                new Pattern() { MorphemeRule = MorphemeRule.Epsilon, RightRule = MorphemeRule.O_Lexeme });
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement));


            adTree = new AdTree(new Morpheme("", 0),
                new Pattern() { RightRule = MorphemeRule.O_Lexeme, }
            );
            adTreeElement = new AdTree(new Morpheme("", 0),
                new Pattern() { MorphemeRule = MorphemeRule.Epsilon, RightRule = MorphemeRule.A_Lexeme });
            Assert.IsFalse(adTree.CanAttachToRight(adTreeElement));
        }

        [Test]
        public void CanAttachToRight_ValencyPosition()
        {
            // The phrase: I read
            AdTree adTree = new AdTree(
                new Morpheme("", 0),
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(1),
                    RightRule = MorphemeRule.I_Lexeme,
                    LeftRule = MorphemeRule.O_Lexeme
                })
                {
                    Right = new AdTree(
                        new Morpheme("read", Attributes.I.Lexeme.Verb),
                        new Pattern()
                        {
                            MorphemeRule = MorphemeRule.I2_Lexeme,
                            RightRule = MorphemeRule.Nothing,
                            LeftRule = MorphemeRule.Nothing,
                        }
                    )
                {
                    Left = new AdTree(
                        new Morpheme("I", Attributes.O.Lexeme.Pronoun),
                        new Pattern()
                        {
                            MorphemeRule = MorphemeRule.O_Lexeme,
                            RightRule = MorphemeRule.Nothing,
                            LeftRule = MorphemeRule.Nothing,
                        }
                    )
                }
            };

            // Try to connect the second valency position.
            AdTree valency2 = new AdTree(
                new Morpheme("", 0),
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(2),
                    RightRule = MorphemeRule.I_Lexeme,
                    LeftRule = MorphemeRule.O_Lexeme
                }
            );

            Assert.IsTrue(valency2.CanAttachToRight(adTree));


            // Try to connect the first valency position.
            AdTree valency1 = new AdTree(
                new Morpheme("", 0),
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(1),
                    RightRule = MorphemeRule.I_Lexeme,
                    LeftRule = MorphemeRule.O_Lexeme
                }
            );

            // The first valency is already there so it should be false.
            Assert.IsFalse(valency1.CanAttachToRight(adTree));

            // Try to connect the third valency position.
            AdTree valency3 = new AdTree(
                new Morpheme("", 0),
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(3),
                    RightRule = MorphemeRule.I_Lexeme,
                    LeftRule = MorphemeRule.O_Lexeme
                }
            );

            // The second valency is missing so it should be false.
            Assert.IsFalse(valency3.CanAttachToRight(adTree));
        }


        [Test]
        public void CanAttachToLeft()
        {
            IAdTree adTree = new AdTree(new Morpheme("", 0),
                new Pattern() { LeftRule = MorphemeRule.O_Lexeme, }
            );
            IAdTree adTreeElement = new AdTree(new Morpheme("", 0),
                new Pattern() { MorphemeRule = MorphemeRule.O_Lexeme });
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement));


            adTree = new AdTree(new Morpheme("", 0),
                new Pattern() { LeftRule = MorphemeRule.O_Lexeme, }
            );
            adTreeElement = new AdTree(new Morpheme("", 0),
                new Pattern() { RightRule = MorphemeRule.A_Lexeme });
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement));
        }

        [Test]
        public void CanAttachToLeft_Inheritance()
        {
            IAdTree adTree = new AdTree(new Morpheme("", 0),
                new Pattern() { LeftRule = MorphemeRule.O_Lexeme, }
            );
            IAdTree adTreeElement = new AdTree(new Morpheme("", 0),
                new Pattern() { MorphemeRule = MorphemeRule.Epsilon, RightRule = MorphemeRule.O_Lexeme });
            Assert.IsTrue(adTree.CanAttachToLeft(adTreeElement));


            // Inheritance is not allowed.
            adTree = new AdTree(new Morpheme("", 0),
                new Pattern() { LeftRule = MorphemeRule.O_Lexeme.SetInheritance(InheritanceRuleMaker.Nothing) }
            );
            adTreeElement = new AdTree(new Morpheme("", 0),
                new Pattern() { MorphemeRule = MorphemeRule.Epsilon, RightRule = MorphemeRule.O_Lexeme });
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement));


            adTree = new AdTree(new Morpheme("", 0),
                new Pattern() { LeftRule = MorphemeRule.O_Lexeme, }
            );
            adTreeElement = new AdTree(new Morpheme("", 0),
                new Pattern() { MorphemeRule = MorphemeRule.Epsilon, RightRule = MorphemeRule.A_Lexeme });
            Assert.IsFalse(adTree.CanAttachToLeft(adTreeElement));
        }


        [Test]
        public void GetFirstAdPositionOnLeft()
        {
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("the", 0), new Pattern())
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

        [Test]
        public void Replace()
        {
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("the", 0), new Pattern())
                }
            };

            AdTree replace = new AdTree(new Morpheme("hello", 0), new Pattern());

            adTree.Right.Replace(replace);

            Assert.IsTrue(adTree.Right == replace);
            Assert.IsTrue(adTree.Right.Right == replace.Right);
            Assert.IsTrue(adTree.Right.Left == replace.Left);
        }

        [Test]
        public void Attach()
        {
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern());
            AdTree toAppend = new AdTree(new Morpheme("hello", 0), new Pattern());
            adTree.Attach(toAppend, AttachPosition.ChildOnLeft);
            Assert.AreEqual("hello", adTree.Left.Morpheme.Morph);
            Assert.IsNull(adTree.Right);


            adTree = new AdTree(new Morpheme("", 0), new Pattern());
            toAppend = new AdTree(new Morpheme("hello", 0), new Pattern());
            adTree.Attach(toAppend, AttachPosition.ChildOnRight);
            Assert.AreEqual("hello", adTree.Right.Morpheme.Morph);
            Assert.IsTrue(adTree.Left == null);
        }

        [Test]
        public void Insert()
        {
            AdTree adTree = new AdTree(new Morpheme("A1", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("A11", 0), new Pattern()),
            };
            AdTree toInsert = new AdTree(new Morpheme("hello", 0), new Pattern());
            adTree.Right.Insert(toInsert, toInsert, AttachPosition.ChildOnLeft);

            Assert.AreEqual("hello", adTree.Right.Morpheme.Morph);
            Assert.AreEqual("A11", adTree.Right.Left.Morpheme.Morph);


            adTree = new AdTree(new Morpheme("A1", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("A11", 0), new Pattern()),
            };
            toInsert = new AdTree(new Morpheme("hello", 0), new Pattern());
            adTree.Right.Insert(toInsert, toInsert, AttachPosition.ChildOnRight);
            
            Assert.AreEqual("hello", adTree.Right.Morpheme.Morph);
            Assert.AreEqual("A11", adTree.Right.Right.Morpheme.Morph);


            // Inserting to the root.
            adTree = new AdTree(new Morpheme("A1", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("A11", 0), new Pattern()),
            };
            toInsert = new AdTree(new Morpheme("hello", 0), new Pattern());
            adTree.Insert(toInsert, toInsert, AttachPosition.ChildOnRight);

            IAdTree root = adTree.Root;
            Assert.AreEqual("hello", root.Morpheme.Morph);
            Assert.AreEqual("A1", root.Right.Morpheme.Morph);
        }

        [Test]
        public void Detach()
        {
            AdTree adTree = new AdTree(new Morpheme("A1", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("A11", 0), new Pattern()),
            };

            adTree.Right.Detach();

            Assert.IsTrue(adTree.Right == null);
        }

        [Test]
        public void Evaluate()
        {
            AdTree adTree = new AdTree(new Morpheme("book", Attributes.O.Lexeme), new Pattern("O")
            {
                MorphemeRule = MorphemeRule.O_Lexeme,
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.Nothing,
            });
            Assert.IsTrue(adTree.Evaluate());

            // note: empty string is not allowed.
            adTree = new AdTree(new Morpheme("", Attributes.O.Lexeme), new Pattern("O")
            {
                MorphemeRule = MorphemeRule.O_Lexeme,
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.Nothing,
            });
            Assert.IsFalse(adTree.Evaluate());

            // note: non-lexeme is not allowed.
            adTree = new AdTree(new Morpheme("bla", Attributes.O.NonLexeme), new Pattern("O")
            {
                MorphemeRule = MorphemeRule.O_Lexeme,
                LeftRule = MorphemeRule.Nothing,
                RightRule = MorphemeRule.Nothing,
            });
            Assert.IsFalse(adTree.Evaluate());


            // Left and right rules are anything so it should also accept if they are null.
            adTree = new AdTree(new Morpheme(".", Attributes.U.NonLexeme), new Pattern("")
            {
                MorphemeRule = MorphemeRule.U_NonLexeme,
                LeftRule = MorphemeRule.Anything,
                RightRule = MorphemeRule.Anything,
            });
            Assert.IsTrue(adTree.Evaluate());
        }

        [Test]
        public void GetNonconformities_MorphemeRule()
        {
            AdTree adTree = new AdTree(
                    new Morpheme("green", Attributes.A.Lexeme),
                    new Pattern("A")
                    {
                        MorphemeRule = MorphemeRule.A_Lexeme,
                        RightRule = MorphemeRule.Nothing,
                        LeftRule = MorphemeRule.Nothing,
                    });

            List<IAdTree> nonconformities = adTree.GetNonconformities().ToList();
            Assert.AreEqual(0, nonconformities.Count);


            // Empty string does not match the rule.
            adTree = new AdTree(
                    new Morpheme("", Attributes.A.Lexeme),
                    new Pattern("A")
                    {
                        MorphemeRule = MorphemeRule.A_Lexeme,
                        RightRule = MorphemeRule.Nothing,
                        LeftRule = MorphemeRule.Nothing,
                    });

            nonconformities = adTree.GetNonconformities().ToList();
            Assert.AreEqual(1, nonconformities.Count);


            // Morpheme attributes does not match the rule.
            adTree = new AdTree(
                    new Morpheme("bla", Attributes.A.NonLexeme),
                    new Pattern("A")
                    {
                        MorphemeRule = MorphemeRule.A_Lexeme,
                        RightRule = MorphemeRule.Nothing,
                        LeftRule = MorphemeRule.Nothing,
                    });

            nonconformities = adTree.GetNonconformities().ToList();
            Assert.AreEqual(1, nonconformities.Count);


            // Attached right children violates the rule.
            adTree = new AdTree(
                    new Morpheme("green", Attributes.A.Lexeme),
                    new Pattern("A")
                    {
                        MorphemeRule = MorphemeRule.A_Lexeme,
                        RightRule = MorphemeRule.Nothing,
                        LeftRule = MorphemeRule.Nothing,
                    })
            {
                // Note: the rule is nothing so having this attached violates the rule.
                Right = new AdTree(new Morpheme("", 0), new Pattern())
            };

            nonconformities = adTree.GetNonconformities().ToList();
            Assert.AreEqual(1, nonconformities.Count);
        }


        [Test]
        public void MakeShallowCopy()
        {
            // The phrase: I read the book.
            IAdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", Attributes.I.Lexeme.Verb), new Pattern()),
                    Left = new AdTree(new Morpheme("I", Attributes.O.Lexeme.Pronoun), new Pattern())
                },
                Left = new AdTree(new Morpheme("", Attributes.U), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", Attributes.O.Lexeme.Noun), new Pattern()),
                    Left = new AdTree(new Morpheme("the", Attributes.A.Lexeme.Determiner), new Pattern())
                }
            };

            // Note: the copy does not start from the root but the whole tree will be copied.
            IAdTree copy = adTree.Right.MakeShallowCopy();

            // The copy must be set to the same position as the original.
            Assert.IsTrue(adTree.Right.Morpheme == copy.Morpheme);
            Assert.IsTrue(adTree.Right.Pattern == copy.Pattern);

            // Get the root - for easier testing.
            adTree = adTree.Root;
            copy = copy.Root;

            Assert.IsTrue(adTree.Morpheme == copy.Morpheme);
            Assert.IsTrue(adTree.Pattern == copy.Pattern);

            Assert.IsTrue(adTree.Right.Morpheme == copy.Right.Morpheme);
            Assert.IsTrue(adTree.Right.Pattern == copy.Right.Pattern);

            Assert.IsTrue(adTree.Right.Right.Morpheme == copy.Right.Right.Morpheme);
            Assert.IsTrue(adTree.Right.Right.Pattern == copy.Right.Right.Pattern);

            Assert.IsTrue(adTree.Right.Left.Morpheme == copy.Right.Left.Morpheme);
            Assert.IsTrue(adTree.Right.Left.Pattern == copy.Right.Left.Pattern);

            Assert.IsTrue(adTree.Left.Morpheme == copy.Left.Morpheme);
            Assert.IsTrue(adTree.Left.Pattern == copy.Left.Pattern);

            Assert.IsTrue(adTree.Left.Right.Morpheme == copy.Left.Right.Morpheme);
            Assert.IsTrue(adTree.Left.Right.Pattern == copy.Left.Right.Pattern);

            Assert.IsTrue(adTree.Left.Left.Morpheme == copy.Left.Left.Morpheme);
            Assert.IsTrue(adTree.Left.Left.Pattern == copy.Left.Left.Pattern);
        }
    }
}
