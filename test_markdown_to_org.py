import unittest
import sys
import os

# 添加当前目录到Python路径
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from markdown_to_org import convert_lists

class TestListConversion(unittest.TestCase):
    def test_asterisk_list(self):
        """测试将*开头的列表转换为-开头的列表"""
        input_text = """* 第一项
* 第二项
  * 子项1
  * 子项2
* 第三项"""
        
        expected_output = """- 第一项
- 第二项
  - 子项1
  - 子项2
- 第三项"""
        
        self.assertEqual(convert_lists(input_text), expected_output)
    
    def test_mixed_lists(self):
        """测试混合类型的列表"""
        input_text = """* 无序列表1
- 无序列表2
1. 有序列表1
2. 有序列表2
  * 子列表1
  - 子列表2"""
        
        expected_output = """- 无序列表1
- 无序列表2
1. 有序列表1
2. 有序列表2
  - 子列表1
  - 子列表2"""
        
        self.assertEqual(convert_lists(input_text), expected_output)
    
    def test_indented_lists(self):
        """测试带缩进的列表"""
        input_text = """  * 缩进项1
    * 缩进项2
      * 缩进项3"""
        
        expected_output = """  - 缩进项1
    - 缩进项2
      - 缩进项3"""
        
        self.assertEqual(convert_lists(input_text), expected_output)

if __name__ == '__main__':
    unittest.main() 