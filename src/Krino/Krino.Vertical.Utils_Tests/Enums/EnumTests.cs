using Krino.Vertical.Utils.Enums;
using NUnit.Framework;
using System.Collections;

namespace Krino.Vertical.Utils_Tests.Enums
{
    [TestFixture]
    public class EnumTests
    {
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

                    public EnumValue Attr111 { get; }
                    public EnumValue Attr112 { get; }
                    public EnumValue Attr113 { get; }
                }

                public DummyCategory1(EnumGroupBase parent) : base(parent)
                {
                    Category11 = new DummyCategory12(this);
                    Val12 = new EnumValue(this);
                }

                public DummyCategory12 Category11 { get; }
                public EnumValue Val12 { get; }
            }



            private static DummyEnumRoot Instance { get; } = new DummyEnumRoot();

            public static DummyCategory1 Category1 { get; } = new DummyCategory1(Instance);

            public static EnumValue Val2 { get; } = new EnumValue(Instance);

        }


        [Test]
        public void EncodedValue_ulong()
        {
            // 1101
            ulong encodedValue = DummyEnumRoot.Category1.Category11.Attr112;
            Assert.AreEqual(0xD0_00_00_00_00_00_00_00, encodedValue);

            // 100001
            encodedValue = DummyEnumRoot.Category1.Val12;
            Assert.AreEqual(0x84_00_00_00_00_00_00_00, encodedValue);

            // 11001
            encodedValue = DummyEnumRoot.Category1.Category11.Attr113;
            Assert.AreEqual(0xC8_00_00_00_00_00_00_00, encodedValue);
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
            ulong encodedValue = DummyEnumRoot.Category1.Category11.Attr112 | DummyEnumRoot.Val2;

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
