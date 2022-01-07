using Krino.Vertical.Utils.Enums;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;
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

                    // 3rd bit ... BitIndex = 2
                    public EnumValue Attr111 { get; }
                    // 4th bit ... BitIndex = 3
                    public EnumValue Attr112 { get; }
                    // 5th bit ... BitIndex = 4
                    public EnumValue Attr113 { get; }
                }

                public DummyCategory1(EnumGroupBase parent) : base(parent)
                {
                    Category11 = new DummyCategory12(this);
                    Val12 = new EnumValue(this);
                }

                // 2nd bit ... BitIndex = 1
                public DummyCategory12 Category11 { get; }
                // 6th bit ... BitIndex = 5
                public EnumValue Val12 { get; }
            }



            public static DummyEnumRoot Instance { get; } = new DummyEnumRoot();

            // 1st bit ... BitIndex = 0
            public static DummyCategory1 Category1 { get; } = new DummyCategory1(Instance);
            // 7th bit ... BitIndex = 6
            public static EnumValue Val2 { get; } = new EnumValue(Instance);

            public static IEnumerable<EnumValue> GetEnumValues() => Instance.EnumValues;

            public static IEnumerable<EnumBase> GetDirectSubEnums() => Instance.DirectSubEnums;

            public static new IEnumerable<EnumBase> FindEnums(BigInteger value) => ((EnumRootBase)Instance).FindEnums(value);
        }

        [Test]
        public void GetName()
        {
            // DummyEnumRoot.(Category1.(Category11.(Attr111,Attr112,Attr113),Val12),Val2)

            var result = DummyEnumRoot.Category1.Category11.Attr112.GetName();
            Assert.AreEqual("Attr112", result);

            result = DummyEnumRoot.Instance.GetName();
            Assert.AreEqual("DummyEnumRoot", result);
        }

        [Test]
        public void GetFullName()
        {
            var result = DummyEnumRoot.Category1.Category11.Attr112.GetFullName();
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.Attr112", result);

            result = DummyEnumRoot.Instance.GetFullName();
            Assert.AreEqual("DummyEnumRoot", result);
        }

        [Test]
        public void GetFullName_ForValue()
        {
            // DummyEnumRoot.(Category1.(Category11.(Attr111,Attr112,Attr113),Val12),Val2)
            BigInteger encodedValue = 0b11111110;
            var result = DummyEnumRoot.Instance.GetFullName(encodedValue);
            Assert.AreEqual("DummyEnumRoot.(Category1.(Category11.(Attr111,Attr112,Attr113),Val12),Val2)", result);

            encodedValue = DummyEnumRoot.Category1.Category11.Attr111 | DummyEnumRoot.Category1.Category11.Attr112;
            result = DummyEnumRoot.Instance.GetFullName(encodedValue);
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.(Attr111,Attr112)", result);

            encodedValue = DummyEnumRoot.Category1.Category11.Attr111;
            result = DummyEnumRoot.Instance.GetFullName(encodedValue);
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.Attr111", result);
        }

        [Test]
        public void HasDirectSubEnums()
        {
            BigInteger value = DummyEnumRoot.Category1.Category11.Attr112;
            Assert.IsTrue(DummyEnumRoot.Category1.HasDirectSubEnums(value));

            value = DummyEnumRoot.Category1;
            Assert.IsFalse(DummyEnumRoot.Category1.HasDirectSubEnums(value));
        }

        [Test]
        public void FindEnums()
        {
            var value = DummyEnumRoot.Category1.Category11.Attr112 | DummyEnumRoot.Category1.Category11.Attr113;
            var result = DummyEnumRoot.FindEnums(value).ToList();
            Assert.AreEqual(2, result.Count);
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.Attr112", result[0].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.Attr113", result[1].GetFullName());

            value = DummyEnumRoot.Category1.Category11 | DummyEnumRoot.Val2;
            result = DummyEnumRoot.FindEnums(value).ToList();
            Assert.AreEqual(2, result.Count);
            Assert.AreEqual("DummyEnumRoot.Category1.Category11", result[0].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Val2", result[1].GetFullName());
        }

        [Test]
        public void Enums()
        {
            var result = DummyEnumRoot.Instance.Enums.ToList();
            Assert.AreEqual(7, result.Count);
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.Attr111", result[0].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.Attr112", result[1].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.Attr113", result[2].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Category1.Category11", result[3].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Category1.Val12", result[4].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Category1", result[5].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Val2", result[6].GetFullName());
        }

        [Test]
        public void EnumValues()
        {
            var result = DummyEnumRoot.GetEnumValues().ToList();
            Assert.AreEqual(5, result.Count);
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.Attr111", result[0].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.Attr112", result[1].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.Attr113", result[2].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Category1.Val12", result[3].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Val2", result[4].GetFullName());
        }

        [Test]
        public void DirectSubEnums()
        {
            var result = DummyEnumRoot.GetDirectSubEnums().ToList();
            Assert.AreEqual(2, result.Count);
            Assert.AreEqual("DummyEnumRoot.Category1", result[0].GetFullName());
            Assert.AreEqual("DummyEnumRoot.Val2", result[1].GetFullName());
        }


        [Test]
        public void GetValue()
        {
            var result = DummyEnumRoot.Instance.GetValue("DummyEnumRoot.Category1");
            var enums = DummyEnumRoot.FindEnums(result).ToList();
            Assert.AreEqual(1, enums.Count);
            Assert.AreEqual("DummyEnumRoot.Category1", enums[0].GetFullName());

            result = DummyEnumRoot.Instance.GetValue("DummyEnumRoot.Category1.Category11.Attr113");
            enums = DummyEnumRoot.FindEnums(result).ToList();
            Assert.AreEqual(1, enums.Count);
            Assert.AreEqual("DummyEnumRoot.Category1.Category11.Attr113", enums[0].GetFullName());

            result = DummyEnumRoot.Instance.GetValue("DummyEnumRoot.Category1.Category11.(Attr111,Attr112)");
            var value = DummyEnumRoot.Category1.Category11.Attr111 | DummyEnumRoot.Category1.Category11.Attr112;
            Assert.AreEqual(value, result);

            result = DummyEnumRoot.Instance.GetValue("DummyEnumRoot.(Category1.(Category11.(Attr111,Attr112,Attr113),Val12),Val2)");
            Assert.AreEqual((BigInteger)254, result);

            // non-existing
            //result = DummyEnumRoot.Instance.GetValue("DummyEnumRoot.Category1.BlaBla");
            //Assert.AreEqual((BigInteger)DummyEnumRoot.Category1, result);

            // non-existing
            result = DummyEnumRoot.Instance.GetValue("DummyEnumRoot.BlaBla");
            Assert.AreEqual((BigInteger)0, result);
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
        public void Clear_Group()
        {
            // Clearing of the sub-group (2, 3, 4 and 5 bit)
            BigInteger encodedValue = 0b11111111;
            var enums = DummyEnumRoot.FindEnums(encodedValue);
            BigInteger result = DummyEnumRoot.Category1.Category11.Clear(encodedValue);
            var enumsResult = DummyEnumRoot.FindEnums(result);
            Assert.AreEqual(0b10000111, (byte)result);
            Assert.IsTrue(DummyEnumRoot.Category1.IsIn(result));
            Assert.IsFalse(DummyEnumRoot.Category1.Category11.IsIn(result));

            // Clearing of top-most group.
            encodedValue = 0b11111111;
            result = DummyEnumRoot.Category1.Clear(encodedValue);
            enumsResult = DummyEnumRoot.FindEnums(result);
            Assert.AreEqual(0b00000011, (byte)result);

            // Clearing of the root.
            encodedValue = 0b11111111;
            result = DummyEnumRoot.Instance.Clear(encodedValue);
            enumsResult = DummyEnumRoot.FindEnums(result);
            Assert.AreEqual(0, (byte)result);
        }

        [Test]
        public void Clear_Value()
        {
            BigInteger encodedValue = 0b11111111;
            var enums = DummyEnumRoot.FindEnums(encodedValue);
            BigInteger result = DummyEnumRoot.Category1.Category11.Attr112.Clear(encodedValue);
            var enumsResult = DummyEnumRoot.FindEnums(result);
            Assert.AreEqual(0b11101111, (byte)result);
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
