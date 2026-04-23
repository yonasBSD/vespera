import { css, Flex, Text, VStack } from '@devup-ui/react'

import { SIDE_MENU_ITEMS } from '@/constants'

import { Effect } from '../header/effect'
import { GnbIcon } from '../header/gnb-icon'
import { SheetContainer } from '../sheet'
import { SideMenu } from '../side-menu'
import { SideMenuProvider } from '../side-menu/side-menu-provider'
import { LightThemeBoundary } from '../theme/light-theme-boundary'
import { ThemeToggle } from '../theme/theme-toggle'
import { SideMenuClickDetector } from './side-menu-click-detector'

export function MobileMenu() {
  return (
    <SheetContainer
      className={css({
        borderRadius: '0px',
        top: '68px',
        overflow: 'auto',
        pb: '$spacingSpacing80',
      })}
      position="right"
    >
      <SideMenuProvider>
        <SideMenuClickDetector
          className={css({
            alignItems: 'center',
            gap: '$spacingSpacing08',
            w: '100%',
          })}
        >
          <VStack
            alignItems="center"
            gap="12px"
            py="$spacingSpacing08"
            w="100%"
          >
            <VStack px="20px" w="100%">
              <Flex
                alignItems="center"
                borderRadius="$spacingSpacing08"
                py="$spacingSpacing12"
              >
                <Text color="$title" flex="1" typography="buttonSm">
                  Documentation
                </Text>
              </Flex>
              {SIDE_MENU_ITEMS.documentation.map(
                ({ value, label, children }) => (
                  <SideMenu key={value} childMenus={children} value={value}>
                    {label}
                  </SideMenu>
                ),
              )}
            </VStack>
            <Flex
              alignItems="center"
              borderRadius="$spacingSpacing08"
              px="20px"
              py="$spacingSpacing12"
              w="100%"
            >
              <Text color="$title" flex="1" typography="buttonSm">
                About us
              </Text>
            </Flex>
          </VStack>
          <Flex alignItems="center">
            <Effect>
              <GnbIcon icon="github" />
            </Effect>
            <Effect>
              <GnbIcon icon="discord" />
            </Effect>
            <Effect>
              <GnbIcon icon="kakao" />
            </Effect>
            <Effect>
              <LightThemeBoundary>
                <ThemeToggle>
                  <GnbIcon icon="theme-light" />
                </ThemeToggle>
              </LightThemeBoundary>
              <LightThemeBoundary reverse>
                <ThemeToggle>
                  <GnbIcon icon="theme-dark" />
                </ThemeToggle>
              </LightThemeBoundary>
            </Effect>
          </Flex>
        </SideMenuClickDetector>
      </SideMenuProvider>
    </SheetContainer>
  )
}
