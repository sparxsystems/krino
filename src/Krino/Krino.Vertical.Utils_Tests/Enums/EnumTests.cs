﻿using Krino.Vertical.Utils.Enums;
using NUnit.Framework;
using System.Numerics;

namespace Krino.Vertical.Utils_Tests.Enums
{
    [TestFixture]
    public class EnumTests
    {
        public class DummyEnumRoot1 : EnumRootBase
        {
            private static DummyEnumRoot1 Instance { get; } = new DummyEnumRoot1();

            public static EnumValue Val1 { get; } = new EnumValue(Instance);

            public static EnumValue Val2 { get; } = new EnumValue(Instance);
        }

        public class DummyEnumRoot : EnumRootBase
        {
            public class DummyCategory1 : EnumGroupBase
            {
                public class DummyCategory12 : EnumGroupBase
                {
                    public DummyCategory12(EnumGroupBase parent) : base(parent)
                    {
                        Attr111 = new EnumValue(this);
                        Attr112 = new EnumValue(this);
                        Attr113 = new EnumValue(this);
                    }

                    // 3rd bit
                    public EnumValue Attr111 { get; }
                    // 4th bit
                    public EnumValue Attr112 { get; }
                    // 5th bit
                    public EnumValue Attr113 { get; }
                }

                public DummyCategory1(EnumGroupBase parent) : base(parent)
                {
                    Category11 = new DummyCategory12(this);
                    Val12 = new EnumValue(this);
                }

                // 2nd bit
                public DummyCategory12 Category11 { get; }
                // 6th bit
                public EnumValue Val12 { get; }
            }



            public static DummyEnumRoot Instance { get; } = new DummyEnumRoot();

            // 1st bit
            public static DummyCategory1 Category1 { get; } = new DummyCategory1(Instance);
            // 7th bit
            public static EnumValue Val2 { get; } = new EnumValue(Instance);
        }

        [Test]
        public void Value()
        {
            BigInteger value1 = DummyEnumRoot1.Val1;
            BigInteger value2 = DummyEnumRoot1.Val2;

            // Bits shall be set from the left to the right.
            // So that the most significant ones have bigger value.
            Assert.AreEqual(0b10000000, (byte)value1);
            Assert.AreEqual(0b01000000, (byte)value2);
        }


        [Test]
        public void BigInteger()
        {
            // 1101
            BigInteger encodedValue = DummyEnumRoot.Category1.Category11.Attr112;
            Assert.AreEqual(0xD0, (byte)encodedValue);

            // 100001
            encodedValue = DummyEnumRoot.Category1.Val12;
            Assert.AreEqual(0x84, (byte)encodedValue);

            // 11001
            encodedValue = DummyEnumRoot.Category1.Category11.Attr113;
            Assert.AreEqual(0xC8, (byte)encodedValue);
        }

        [Test]
        public void Clear()
        {
            // Clearing of the sub-group (2, 3, 4 and 5 bit)
            BigInteger encodedValue = 0b11111111;
            BigInteger result = DummyEnumRoot.Category1.Category11.Clear(encodedValue);
            Assert.AreEqual(0b10000111, (byte)result);

            // Clearing of top-most group.
            encodedValue = 0b11111111;
            result = DummyEnumRoot.Category1.Clear(encodedValue);
            Assert.AreEqual(0b00000011, (byte)result);

            // Clearing of the root.
            encodedValue = 0b11111111;
            result = DummyEnumRoot.Instance.Clear(encodedValue);
            Assert.AreEqual(0, (byte)result);
        }

        [Test]
        public void EncodedValue_comparing()
        {
            Assert.IsTrue(DummyEnumRoot.Category1.Category11.Attr111 > DummyEnumRoot.Category1.Category11.Attr112);
            Assert.IsTrue(DummyEnumRoot.Category1.Category11.Attr112 > DummyEnumRoot.Category1.Category11.Attr113);
            Assert.IsTrue(DummyEnumRoot.Category1.Category11.Attr113 > DummyEnumRoot.Category1.Category11);
            Assert.IsTrue(DummyEnumRoot.Category1.Category11 > DummyEnumRoot.Category1.Val12);
            Assert.IsTrue(DummyEnumRoot.Category1.Val12 > DummyEnumRoot.Category1);
            Assert.IsTrue(DummyEnumRoot.Category1 > DummyEnumRoot.Val2);
        }

        [Test]
        public void IsIn()
        {
            BigInteger encodedValue = DummyEnumRoot.Category1.Category11.Attr112 | DummyEnumRoot.Val2;

            Assert.IsTrue(DummyEnumRoot.Category1.IsIn(encodedValue));
            Assert.IsTrue(DummyEnumRoot.Category1.Category11.IsIn(encodedValue));
            Assert.IsTrue(DummyEnumRoot.Category1.Category11.Attr112.IsIn(encodedValue));

            Assert.IsTrue(DummyEnumRoot.Val2.IsIn(encodedValue));

            Assert.IsFalse(DummyEnumRoot.Category1.Val12.IsIn(encodedValue));
            Assert.IsFalse(DummyEnumRoot.Category1.Category11.Attr111.IsIn(encodedValue));
            Assert.IsFalse(DummyEnumRoot.Category1.Category11.Attr113.IsIn(encodedValue));
        }
    }
}
