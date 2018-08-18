#include <vector>
#include <string>
#include <iostream>
#include <cstdlib>

struct MyPointer {
  int i;
  std::string s;
  MyPointer(int i, const std::string & s)
    : i(i)
    , s(s)
  {}
};

std::string toString(const MyPointer& ptr) {
  return "MyPointer{" + std::to_string(ptr.i) + ", " + ptr.s + "}";
}

int logIndex = 0;
std::vector<std::string> theLog;

void log(std::string s) {
  std::cerr << "NATIVE: " << s << std::endl;
  theLog.push_back(s);
}

extern "C" {

  const char* nextLog() {
    if (logIndex < theLog.size()) {
      // do not delete, this simplifies memory management
      return theLog[logIndex++].c_str();
    } else {
      return "EMPTY";
    }
  }

  void clearLog() {
    theLog.clear();
  }

  void fnVoidVoid() {
    log("fnVoidVoid()");
  }

  void fnVoidInt(int x) {
    log("fnVoidInt(" + std::to_string(x) + ")");
  }

  int fnIntInt(int x) {
    int result = rand();
    log("fnIntInt(" + std::to_string(x) + ")=" + std::to_string(result));
    return result;
  }

  int fnEnumEnum(int x) {
    auto result = rand() % 2;
    log("fnEnumEnum(" + std::to_string(x) + ")=" + std::to_string(result));
    return result;
  }
  
  MyPointer* createRandomMyPointer() {
    MyPointer* result = new MyPointer { rand(), "s" + std::to_string(rand())};
    log("createRandomMyPointer()=" + toString(*result));
    return result;
  }

  MyPointer* createMyPointer(int i, const char* s) {
    MyPointer* result = new MyPointer {i, s};
    log("createMyPointer(" + std::to_string(i) + ", " + s + ")=" + toString(*result));
    return result;
  }

  void disposeMyPointer(MyPointer* ptr) {
    log("disposeMyPointer(" + toString(*ptr) +")");
    delete ptr;
  }

  int myPointerGetI(MyPointer* s) {
    log("myStructGetI(" + toString(*s) + ")=" + std::to_string(s->i));
    return s->i;
  }

  const char* myPointerGetS(MyPointer* s) {
    log("myStructGetS(" + toString(*s)+ ")=" + s->s);
    return s->s.c_str();
  }

  void randomizeMyPointerArray(int n, MyPointer** result) {
    std::string l = "randomizeMyPointersArray(" + std::to_string(n) + ")=[";
    bool first = true;
    for (int i = 0; i < n; ++i) {
      if (!first) {
	l = l + ", ";
      }
      result[i] = (MyPointer*) createRandomMyPointer();
      l = toString(*result[i]);
      first = false;
    }
    l = l + "]";
    log(l);
  }

  MyPointer* getMyPointerInArray(MyPointer** array, int i) {
    auto result = array[i];
    log("getMyPointerInArray[ARRAY, " + std::to_string(i) + "]=" + toString(*result));
    return result;
  }

  void setMyPointerInArray(MyPointer** a, int i, MyPointer* ptr) {
    log("setMyPointerInArray[ARRAY, " + std::to_string(i) + ", " + toString(*ptr) + ")");
    MyPointer** array = (MyPointer**) a;
    array[i] = ptr;
  }

  int sum(int* array, int n) {
    int result = 0;
    for (int i = 0; i < n; ++i) {
      result += array[i];
    }
    return result;
  }

  void fillMultiples(int n, int k, int* result) {
    for (int i = 0; i < n; ++i) {
      result[i] = k*i;
    }
  }

  void invertEnumArray(int n, int* array) {
    for (int i = 0; i < n; ++i) {
      array[i] = array[i] == 1 ? 0 : 1;
    }
  }

  const char* concat(const char** strings, int n) {
    std::string* output = new std::string();
    for (int i = 0; i < n; ++i) {
      *output += strings[i];
    }
    return output->c_str();
  }

  void createMultiplesAsString(int n, int k, char** output) {
    for (int i = 0; i < n; ++i) {
      output[i] = (char*) (new std::string(std::to_string(k*i)))->c_str();
    }
  }

  // TODO callbacks
  
}
